' vtirc.bas -- VTIRC minimal IRC client built on libvt
' FreeBASIC 1.10.1 | Windows + Linux
#cmdline "-s gui -gen gcc -O 2"
#Define VT_USE_NET
#Define VT_USE_TUI
#Include Once "vt/vt.bi"

Const VERSION      = "1.1.0"
Const HISTORY_MAX  = 2000
Const CHAT_TOP_ROW = 2
Const CHAT_BOT_ROW = 37  '22
Const CHAT_ROWS    = 36  '21
Const ROW_INPUT    = 39  '24
Const ROW_STATUS   = 40  '25
Const SCREEN_COLS  = 100 '80
Const SCREEN_ROWS  = 40  '25
' F2 server settings form
Const FORM_W       = 46
Const FORM_H       = 13
Const FORM_X       = (SCREEN_COLS - FORM_W) \ 2
Const FORM_Y       = (SCREEN_ROWS - FORM_H) \ 2
' F3 common settings form
Const CFORM_W      = 40
Const CFORM_H      = 13
Const CFORM_X      = (SCREEN_COLS - CFORM_W) \ 2
Const CFORM_Y      = (SCREEN_ROWS - CFORM_H) \ 2
' Window array
Const WIN_MAX      = 16

Type irc_config
    server         As String
    port           As Long
    channel        As String
    nick           As String
    password       As String
    scheme         As Byte      ' 0=Dark  1=Classic  2=Light
    log_enabled    As Byte
    log_pm         As Byte      ' 0=off  1=log PM windows to file
    auto_reconnect As Byte      ' 0=off  1=reconnect on unexpected drop
End Type

Type irc_line
    txt    As String
    col_fg As UByte
End Type

Type irc_window
    target                   As String    ' "#channel" or "nick" (PRIVMSG routing)
    is_pm                    As Byte      ' 0 = channel, 1 = PM
    unread                   As Byte      ' 1 = new messages arrived while not active
    history(HISTORY_MAX - 1) As irc_line
    hist_count               As Long
    hist_head                As Long      ' ring buffer head (oldest entry index)
    top_line                 As Long      ' scroll offset (0 = live bottom)
    new_msgs                 As Byte      ' 1 when history grows while scrolled up
End Type

Dim Shared cfg           As irc_config
Dim Shared cfg_file      As String
Dim Shared input_form(0) As vt_tui_form_item
Dim Shared input_focused As Long
Dim Shared connected     As Byte   ' 1 after 001 welcome received
Dim Shared sock_valid    As Byte   ' 1 after socket opened and connected
Dim Shared quit_flag     As Byte
Dim Shared sock          As SOCKET
Dim Shared recv_buf      As String
Dim Shared is_afk        As Byte
Dim Shared afk_msg       As String

' auto-reconnect state
Const RECONNECT_DELAY = 5.0                   ' seconds between drop and retry
Dim Shared reconnect_pending As Byte   = 0    ' 1 = waiting to reconnect
Dim Shared reconnect_at      As Double = 0    ' Timer value when to fire
Dim Shared is_reconnect      As Byte   = 0    ' signals irc_connect to keep history

' active colour scheme -- set once at startup and after settings OK
Dim Shared As Ubyte col_bg_main, col_fg_body, col_fg_own, col_fg_other, _
                    col_fg_sys, col_bar_fg, col_bar_bg

Const USER_MAX = 512
Dim Shared user_list(USER_MAX - 1) As String
Dim Shared user_count              As Long = 0
Dim Shared names_receiving         As Byte = 0

Dim Shared wins(WIN_MAX - 1) As irc_window
Dim Shared win_count         As Long = 1   ' window 0 = channel, always open
Dim Shared active_win        As Long = 0

Declare Sub irc_poll()
Declare Sub irc_on_drop()
Declare Function win_find(tgt As String) As Long
Declare Function win_open(tgt As String, pm As Byte) As Long
Declare Sub win_hist_append(win_idx As Long, txt As String, col_fg As UByte)
Declare Sub win_close_pm(win_idx As Long)
Declare Sub user_list_form()
Declare Function irc_strip_colors(s As String) As String
Declare Function mirc_wordwrap(txt As String, wid As Long) As String

' Close callback
Function on_close() As Byte
    quit_flag = 1
    Return 1
End Function

' Colour scheme
Sub scheme_apply()
    Select Case cfg.scheme
    Case 1  ' Classic
        col_bg_main  = VT_BLACK      : col_fg_body  = VT_LIGHT_GREY
        col_fg_own   = VT_WHITE      : col_fg_other = VT_YELLOW
        col_fg_sys   = VT_GREEN      : col_bar_fg   = VT_BLACK  : col_bar_bg = VT_CYAN
    Case 2  ' Light
        col_bg_main  = VT_WHITE      : col_fg_body  = VT_BLACK
        col_fg_own   = VT_BLUE       : col_fg_other = VT_RED
        col_fg_sys   = VT_DARK_GREY  : col_bar_fg   = VT_BLACK  : col_bar_bg = VT_CYAN
    Case Else  ' Dark (0, default)
        col_bg_main  = VT_BLACK      : col_fg_body  = VT_LIGHT_GREY
        col_fg_own   = VT_WHITE      : col_fg_other = VT_BRIGHT_CYAN
        col_fg_sys   = VT_DARK_GREY  : col_bar_fg   = VT_BLACK  : col_bar_bg = VT_LIGHT_GREY
    End Select
End Sub

Function nick_color(nick_str As String) As UByte
    Dim h As ULong = 5381
    Dim i As Long
    For i = 1 To Len(nick_str)
        h = ((h Shl 5) + h) Xor Asc(nick_str, i)
    Next i
    Dim pal(0 To 5) As UByte = { _
        VT_BRIGHT_CYAN, VT_YELLOW, VT_BRIGHT_GREEN, _
        VT_BRIGHT_MAGENTA, VT_BRIGHT_BLUE, VT_BRIGHT_RED }
    Return pal(h Mod 6)
End Function

' Logging  (open-write-close every call so the file is always flushed)
Sub log_write(txt As String)
    If cfg.log_enabled = 0 Then Exit Sub
    If Len(cfg.server) = 0 OrElse Len(cfg.channel) = 0 Then Exit Sub
    Dim ch_safe  As String = cfg.channel
    If Left(ch_safe, 1) = "#" Then ch_safe = Mid(ch_safe, 2)
    Dim log_path As String = ExePath() & "/" & cfg.server & "_" & ch_safe & ".log"
    Dim f        As Long   = FreeFile()
    Open log_path For Append As #f
    Print #f, "[" & Date() & " " & Time() & "] " & txt
    Close #f
End Sub

Sub log_write_pm(tgt As String, txt As String)
    If cfg.log_pm = 0 Then Exit Sub
    If Len(cfg.server) = 0 OrElse Len(tgt) = 0 Then Exit Sub
    Dim log_path As String = ExePath() & "/" & cfg.server & "_" & tgt & "_pm.log"
    Dim f        As Long   = FreeFile()
    Open log_path For Append As #f
    Print #f, "[" & Date() & " " & Time() & "] " & txt
    Close #f
End Sub

' -----------------------------------------------------------------------------
' History  (ring buffer + word-wrap at append time)
' Per-window: win_hist_append writes into wins(win_idx).
' hist_append is a convenience wrapper that routes to window 0 (channel).
' All existing callsites use hist_append and remain unchanged.
' -----------------------------------------------------------------------------

' Core ring-buffer append.  Handles word-wrap, log (channel only), unread flag.
Sub win_hist_append(win_idx As Long, txt As String, col_fg As UByte)
    If win_idx < 0 OrElse win_idx >= win_count Then Exit Sub
    ' log stripped text: channel window -> channel file, PM window -> pm file
    If win_idx = 0 Then
        log_write(irc_strip_colors(txt))
    ElseIf cfg.log_pm AndAlso wins(win_idx).is_pm Then
        log_write_pm(wins(win_idx).target, irc_strip_colors(txt))
    End If
    ' word-wrap by visual width (color codes are transparent to column counter)
    Dim wrapped    As String = mirc_wordwrap(txt, SCREEN_COLS - 2)
    Dim parts()    As String
    Dim part_count As Long = vt_str_split(wrapped, Chr(10), parts())
    Dim i    As Long
    Dim slot As Long
    For i = 0 To part_count - 1
        If Len(parts(i)) = 0 Then Continue For
        If wins(win_idx).hist_count < HISTORY_MAX Then
            slot = (wins(win_idx).hist_head + wins(win_idx).hist_count) Mod HISTORY_MAX
            wins(win_idx).history(slot).txt    = parts(i)
            wins(win_idx).history(slot).col_fg = col_fg
            wins(win_idx).hist_count += 1
        Else
            wins(win_idx).history(wins(win_idx).hist_head).txt    = parts(i)
            wins(win_idx).history(wins(win_idx).hist_head).col_fg = col_fg
            wins(win_idx).hist_head = (wins(win_idx).hist_head + 1) Mod HISTORY_MAX
        End If
    Next i
    ' set scroll alert only when this window is not the one on screen
    If win_idx <> active_win Then
        wins(win_idx).unread = 1
    Else
        If wins(win_idx).top_line > 0 Then wins(win_idx).new_msgs = 1
    End If
End Sub

' Convenience wrapper -- routes to window 0 (channel).
' All callsites that currently call hist_append() continue to work unchanged.
Sub hist_append(txt As String, col_fg As UByte)
    win_hist_append(0, txt, col_fg)
End Sub

' -----------------------------------------------------------------------------
' Config
' -----------------------------------------------------------------------------
Sub cfg_defaults()
    cfg.server      = "irc.libera.chat"
    cfg.port        = 6667
    cfg.channel     = "#freebasic"
    cfg.nick        = "VTIRCuser"
    cfg.password    = ""
    cfg.scheme      = 0
    cfg.log_enabled = 0
    cfg.log_pm      = 0
    cfg.auto_reconnect = 0
End Sub

Function cfg_load() As Byte
    If vt_file_exists(cfg_file) = 0 Then Return 0
    Dim f       As Long = FreeFile()
    Dim ln      As String
    Dim sep_pos As Long
    Open cfg_file For Input As #f
    Do While( EOF(f) = 0 )
        Line Input #f, ln
        ln      = Trim(ln)
        sep_pos = InStr(ln, "=")
        If sep_pos = 0 Then Continue Do
        Dim cfg_key As String = LCase(Left(ln, sep_pos - 1))
        Dim cfg_val As String = Mid(ln, sep_pos + 1)
        Select Case cfg_key
        Case "server"      : cfg.server      = cfg_val
        Case "port"        : cfg.port        = Val(cfg_val)
        Case "channel"     : cfg.channel     = cfg_val
        Case "nick"        : cfg.nick        = cfg_val
        Case "password"    : cfg.password    = cfg_val
        Case "scheme"      : cfg.scheme      = Val(cfg_val)
        Case "log_enabled" : cfg.log_enabled = Val(cfg_val)
        Case "log_pm"      : cfg.log_pm      = Val(cfg_val)
        Case "auto_reconnect" : cfg.auto_reconnect = Val(cfg_val)
        End Select
    Loop
    Close #f
    Return 1
End Function

Sub cfg_save()
    Dim f As Long = FreeFile()
    Open cfg_file For Output As #f
    Print #f, "server="      & cfg.server
    Print #f, "port="        & cfg.port
    Print #f, "channel="     & cfg.channel
    Print #f, "nick="        & cfg.nick
    Print #f, "password="    & cfg.password
    Print #f, "scheme="         & cfg.scheme
    Print #f, "log_enabled="    & cfg.log_enabled
    Print #f, "log_pm="         & cfg.log_pm
    Print #f, "auto_reconnect=" & cfg.auto_reconnect
    Close #f
End Sub

' -----------------------------------------------------------------------------
' Draw  (all subs read colours directly from shared col_* vars)
' -----------------------------------------------------------------------------
Sub draw_titlebar()
    Dim lbl_left  As String = " [ VTIRC " & VERSION & "]"
    Dim lbl_right As String = "[ " & cfg.server & " / " & wins(active_win).target & " ] "
    Dim gap       As Long   = SCREEN_COLS - Len(lbl_left) - Len(lbl_right)
    If gap < 0 Then gap = 0
    vt_color(col_bar_fg, col_bar_bg)
    vt_locate(1, 1)
    vt_print(lbl_left & Space(gap) & lbl_right)
End Sub

' -----------------------------------------------------------------------------
' mIRC color rendering helpers
' -----------------------------------------------------------------------------

' Map mIRC color index 0-15 to VT color constant.
' mIRC palette: 0=white 1=black 2=navy 3=green 4=red 5=maroon
'               6=purple 7=orange 8=yellow 9=ltgreen 10=teal
'               11=ltcyan 12=ltblue 13=pink 14=grey 15=ltgrey
Function mirc_to_vt(mirc_idx As Long) As UByte
    Static lut(15) As UByte = { _
        VT_WHITE,          _  ' 0  white
        VT_BLACK,          _  ' 1  black
        VT_BLUE,           _  ' 2  navy blue
        VT_GREEN,          _  ' 3  green
        VT_BRIGHT_RED,     _  ' 4  red
        VT_RED,            _  ' 5  maroon / dark red
        VT_MAGENTA,        _  ' 6  purple
        VT_BROWN,          _  ' 7  orange / brown
        VT_YELLOW,         _  ' 8  yellow
        VT_BRIGHT_GREEN,   _  ' 9  light green
        VT_CYAN,           _  ' 10 teal / dark cyan
        VT_BRIGHT_CYAN,    _  ' 11 light cyan
        VT_BRIGHT_BLUE,    _  ' 12 light blue
        VT_BRIGHT_MAGENTA, _  ' 13 pink / light magenta
        VT_DARK_GREY,      _  ' 14 grey
        VT_LIGHT_GREY }       ' 15 light grey
    If mirc_idx < 0  Then mirc_idx = 0
    If mirc_idx > 15 Then mirc_idx = 15
    Return lut(mirc_idx)
End Function

' Return the number of printable (visible) characters in s,
' skipping mIRC color codes and other formatting control bytes.
Function mirc_visual_len(s As String) As Long
    Dim i    As Long = 1
    Dim slen As Long = Len(s)
    Dim vlen As Long = 0
    Dim ch   As UByte
    Dim d    As Long
    Do While i <= slen
        ch = Asc(s, i)
        Select Case ch
        Case 3   ' Chr(3)[fg[,bg]] color code
            i += 1
            d  = 0
            Do While i <= slen AndAlso d < 2
                If Asc(s, i) >= 48 AndAlso Asc(s, i) <= 57 Then
                    i += 1 : d += 1
                Else
                    Exit Do
                End If
            Loop
            If i <= slen AndAlso Asc(s, i) = 44 Then
                i += 1 : d = 0
                Do While i <= slen AndAlso d < 2
                    If Asc(s, i) >= 48 AndAlso Asc(s, i) <= 57 Then
                        i += 1 : d += 1
                    Else
                        Exit Do
                    End If
                Loop
            End If
        Case 2, 15, 22, 31   ' bold / reset / reverse / underline -- no glyph
            i += 1
        Case Else
            vlen += 1
            i    += 1
        End Select
    Loop
    Return vlen
End Function

' Word-wrap txt at word boundaries by *visual* width (mIRC control bytes
' are transparent to the column counter).  Returns segments joined with Chr(10).
' Words longer than wid are kept intact on their own line.
Function mirc_wordwrap(txt As String, wid As Long) As String
    Dim result   As String
    Dim cur_line As String   ' line being assembled (raw, includes control codes)
    Dim cur_vis  As Long = 0 ' visual column count of cur_line
    Dim wrd      As String   ' word being assembled
    Dim wrd_vis  As Long = 0 ' visual length of wrd
    Dim i        As Long = 1
    Dim slen     As Long = Len(txt)
    Dim ch       As UByte
    Dim d        As Long

    Do While i <= slen
        ch = Asc(txt, i)
        Select Case ch
        Case 3   ' color code -- absorb into current word, no visual cost
            wrd &= Chr(3) : i += 1 : d = 0
            Do While i <= slen AndAlso d < 2
                If Asc(txt, i) >= 48 AndAlso Asc(txt, i) <= 57 Then
                    wrd &= Chr(Asc(txt, i)) : i += 1 : d += 1
                Else
                    Exit Do
                End If
            Loop
            If i <= slen AndAlso Asc(txt, i) = 44 Then
                wrd &= "," : i += 1 : d = 0
                Do While i <= slen AndAlso d < 2
                    If Asc(txt, i) >= 48 AndAlso Asc(txt, i) <= 57 Then
                        wrd &= Chr(Asc(txt, i)) : i += 1 : d += 1
                    Else
                        Exit Do
                    End If
                Loop
            End If
        Case 2, 15, 22, 31   ' other formatting -- absorb, no visual cost
            wrd &= Chr(ch) : i += 1
        Case 32   ' space = word boundary
            ' decide whether wrd fits on cur_line
            If cur_vis > 0 AndAlso cur_vis + wrd_vis > wid Then
                ' flush cur_line, start fresh
                If Len(result) > 0 Then result &= Chr(10)
                result   &= cur_line
                cur_line  = wrd
                cur_vis   = wrd_vis
            Else
                cur_line &= wrd
                cur_vis  += wrd_vis
            End If
            wrd = "" : wrd_vis = 0
            ' append the space if room remains
            If cur_vis < wid Then
                cur_line &= " " : cur_vis += 1
            End If
            i += 1
        Case Else   ' visible character
            wrd     &= Chr(ch)
            wrd_vis += 1
            i       += 1
        End Select
    Loop
    ' flush trailing word
    If Len(wrd) > 0 Then
        If cur_vis > 0 AndAlso cur_vis + wrd_vis > wid Then
            If Len(result) > 0 Then result &= Chr(10)
            result   &= cur_line
            cur_line  = wrd
        Else
            cur_line &= wrd
        End If
    End If
    ' flush trailing line
    If Len(cur_line) > 0 Then
        If Len(result) > 0 Then result &= Chr(10)
        result &= cur_line
    End If
    Return result
End Function

' Render one history line at terminal row ln_row, starting at column 2.
Sub draw_mirc_line(ln_row As Long, base_fg As UByte, base_bg As UByte, _
                   txt As String, max_vis As Long)
    Dim cur_fg  As UByte = base_fg
    Dim cur_bg  As UByte = base_bg
    Dim rev_vid As Byte  = 0          ' Chr(22) reverse-video toggle
    Dim prt_buf As String
    Dim i       As Long  = 1
    Dim slen    As Long  = Len(txt)
    Dim vis     As Long  = 0
    Dim ch      As UByte
    Dim d       As Long
    Dim nfg     As Long
    Dim nbg     As Long
    Dim dstr    As String

    vt_locate(ln_row, 2)
    vt_color(cur_fg, cur_bg)          ' no reverse yet, straight apply

    Do While i <= slen AndAlso vis < max_vis
        ch = Asc(txt, i)
        Select Case ch
        Case 3   ' mIRC color -- flush buffer then update color
            If Len(prt_buf) > 0 Then vt_print(prt_buf) : prt_buf = ""
            i += 1 : nfg = -1 : nbg = -1
            dstr = "" : d = 0
            Do While i <= slen AndAlso d < 2
                If Asc(txt, i) >= 48 AndAlso Asc(txt, i) <= 57 Then
                    dstr &= Chr(Asc(txt, i)) : i += 1 : d += 1
                Else
                    Exit Do
                End If
            Loop
            If Len(dstr) > 0 Then nfg = Val(dstr)
            If i <= slen AndAlso Asc(txt, i) = 44 Then
                i += 1 : dstr = "" : d = 0
                Do While i <= slen AndAlso d < 2
                    If Asc(txt, i) >= 48 AndAlso Asc(txt, i) <= 57 Then
                        dstr &= Chr(Asc(txt, i)) : i += 1 : d += 1
                    Else
                        Exit Do
                    End If
                Loop
                If Len(dstr) > 0 Then nbg = Val(dstr)
            End If
            ' bare Chr(3) with no digits resets colors (but not the reverse flag)
            If nfg >= 0 Then
                cur_fg = mirc_to_vt(nfg)
            Else
                cur_fg = base_fg : cur_bg = base_bg
            End If
            If nbg >= 0 Then cur_bg = mirc_to_vt(nbg)
            If rev_vid Then vt_color(cur_bg, cur_fg) Else vt_color(cur_fg, cur_bg)
        Case 15   ' mIRC plain reset -- clears colors AND reverse flag
            If Len(prt_buf) > 0 Then vt_print(prt_buf) : prt_buf = ""
            cur_fg  = base_fg : cur_bg = base_bg : rev_vid = 0
            vt_color(cur_fg, cur_bg)
            i += 1
        Case 22   ' reverse video toggle
            If Len(prt_buf) > 0 Then vt_print(prt_buf) : prt_buf = ""
            rev_vid Xor= 1
            If rev_vid Then vt_color(cur_bg, cur_fg) Else vt_color(cur_fg, cur_bg)
            i += 1
        Case 2, 31  ' bold / underline -- skip silently
            i += 1
        Case Else   ' printable character
            prt_buf &= Chr(ch)
            vis     += 1
            i       += 1
        End Select
    Loop
    If Len(prt_buf) > 0 Then vt_print(prt_buf)
End Sub

' reads history from the active window's ring buffer.
' Scroll state (top_line) is also per-window -- switching windows preserves
' each window's individual scroll position.
Sub draw_history()
    Dim row      As Long
    Dim i        As Long
    Dim ri       As Long
    Dim txt_disp As String
    vt_tui_rect_fill(1, CHAT_TOP_ROW, SCREEN_COLS, CHAT_ROWS, 32, VT_LIGHT_GREY, col_bg_main)
    Dim w_hc  As Long = wins(active_win).hist_count
    Dim w_hh  As Long = wins(active_win).hist_head
    Dim w_tl  As Long = wins(active_win).top_line
    Dim first As Long = w_hc - CHAT_ROWS - w_tl
    If first < 0 Then first = 0
    Dim last  As Long = w_hc - 1 - w_tl
    If last < 0 Then Return
    row = CHAT_TOP_ROW
    For i = first To last
        If row > CHAT_BOT_ROW Then Exit For
        ri       = (w_hh + i) Mod HISTORY_MAX
        txt_disp = wins(active_win).history(ri).txt
        draw_mirc_line(row, wins(active_win).history(ri).col_fg, col_bg_main, txt_disp, SCREEN_COLS - 2)
        row += 1
    Next i
End Sub

Sub draw_status()
    Dim hint     As String
    Dim uw_i     As Long
    Dim pm_alert As String = ""
    For uw_i = 1 To win_count - 1
        If wins(uw_i).unread Then
            If Len(pm_alert) = 0 Then pm_alert = " [PM:"
            pm_alert &= " " & wins(uw_i).target
        End If
    Next uw_i
    If Len(pm_alert) > 0 Then
        pm_alert &= "] Ctrl+Tab"
        hint      = pm_alert
    End If
    ' left-side indicators -- keep afk_msg capped so bar never overflows
    If is_afk Then
        hint &= " [AFK: " & Left(afk_msg, 10) & "]"   ' append, don't overwrite
    End If
    
    If cfg.log_enabled Then
        hint &= " [LOG]"
    End If
    ' right-side context hint -- per-window scroll state
    If wins(active_win).top_line > 0 AndAlso wins(active_win).new_msgs Then
        hint &= " (new messages below)  PgDn / End = Bottom  |  F1=Help"
    Else
        hint &= " F1=Help"
    End If
    vt_color(col_bar_fg, col_bar_bg)
    vt_locate(ROW_STATUS, 1)
    vt_print(vt_str_pad_right(hint, SCREEN_COLS, " "))
End Sub

Sub draw_input()
    ' nick prefix may change at runtime (/nick) so recalculate x/wid every frame
    Dim prefix  As String = cfg.nick & "> "
    Dim pfx_len As Long   = Len(prefix)
    vt_color(col_fg_body, col_bg_main)
    vt_locate(ROW_INPUT, 1)
    vt_print(prefix)
    input_form(0).x   = pfx_len + 1
    input_form(0).y   = ROW_INPUT
    input_form(0).wid = SCREEN_COLS - pfx_len
    ' form draw must be the last draw call -- sets cursor position
    vt_tui_form_draw(input_form(), input_focused)
End Sub

Sub draw_all()
    vt_color(col_fg_body, col_bg_main)
    vt_cls(col_bg_main)
    draw_titlebar()
    draw_history()
    draw_status()
    draw_input()   ' last -- owns the cursor via vt_tui_form_draw
End Sub

' Help window  (scrollable readonly editor, Esc or Close button to exit)
Sub help_window()
    Const HW   = 44
    Const HH   = 17
    Const HX   = (SCREEN_COLS - HW) \ 2
    Const HY   = (SCREEN_ROWS - HH) \ 2
    Const ED_X = HX + 2
    Const ED_Y = HY + 2
    Const ED_W = HW - 4
    Const ED_H = HH - 5

    Dim help_txt As String
    help_txt  = " Keyboard Shortcuts"                    & VT_LF
    help_txt &= "  F1          This help"                & VT_LF
    help_txt &= "  F2          Server settings"          & VT_LF
    help_txt &= "  F3          Colour / Log settings"    & VT_LF
    help_txt &= "  F4          User list / PM"           & VT_LF
    help_txt &= "  F10         Disconnect"               & VT_LF
    help_txt &= "  Alt+F4      Quit"                     & VT_LF
    help_txt &= "  Ctrl+Tab    Cycle windows"            & VT_LF
    help_txt &= "  Ctrl+W      Close PM window"          & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Scrolling"                             & VT_LF
    help_txt &= "  PgUp/PgDn/Wheel  Scroll history"      & VT_LF
    help_txt &= "  Home             Jump to oldest"      & VT_LF
    help_txt &= "  End              Jump to newest"      & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Copy from history"                     & VT_LF
    help_txt &= "  1)hold LMB and drag to select"        & VT_LF
    help_txt &= "  2)RMB to copy selection to clipboard" & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Paste from clipboard"                  & VT_LF
    help_txt &= "  MMB in chat line or SHIFT+INS"        & VT_LF
    help_txt &= ""                                       & VT_LF  
    help_txt &= " Commands"                              & VT_LF
    help_txt &= "  /nick <n>        Change nickname"     & VT_LF
    help_txt &= "  /names           List channel users"  & VT_LF
    help_txt &= "  /msg <n> [txt]   Open PM window"      & VT_LF
    help_txt &= "  /afk [msg]       Set away status"     & VT_LF
    help_txt &= "  /back            Return from AFK"     & VT_LF
    help_txt &= "  /join <ch>       Join channel"        & VT_LF
    help_txt &= "  /part            Leave channel"       & VT_LF
    help_txt &= "  /quit            Disconnect"          & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Notices"                               & VT_LF
    help_txt &= "  Notices from NickServ/services"       & VT_LF
    help_txt &= "  appear as -nick- message in yellow"   & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Color Input (EGA indexes)"             & VT_LF
    help_txt &= "  ^ 0=blk   ^1=blu   ^2=grn   ^3=cyn"   & VT_LF
    help_txt &= "  ^ 4=red   ^5=mag   ^6=brn   ^7=lgr"   & VT_LF
    help_txt &= "  ^ 8=dgr   ^9=bblu ^10=bgrn ^11=bcyn"  & VT_LF
    help_txt &= "  ^12=bred ^13=bmag ^14=yel  ^15=wht"

    Dim ed_st As vt_tui_editor_state
    ed_st.work   = help_txt
    ed_st.cpos   = 0
    ed_st.top_ln = 0
    ed_st.dirty  = 0
    ed_st.flags  = VT_TUI_ED_READONLY

    Dim hlp_items(0) As vt_tui_form_item
    Dim hlp_focused  As Long = 0
    hlp_items(0).kind = VT_FORM_BUTTON
    hlp_items(0).x    = (HW - 9) \ 2
    hlp_items(0).y    = HH - 2
    hlp_items(0).val  = "Close"
    hlp_items(0).ret  = 1
    vt_tui_form_offset(hlp_items(), HX, HY)

    Dim k      As ULong
    Dim result As Long

    draw_all()

    Do
        irc_poll()
        k      = vt_inkey()
        result = vt_tui_form_handle(hlp_items(), hlp_focused, k, VT_FORM_NO_ESC)
        vt_tui_editor_handle(ED_X, ED_Y, ED_W, ED_H, ed_st, k)

        If VT_SCAN(k) = VT_KEY_ESC Then Exit Do
        If result = 1 Then Exit Do

        vt_tui_rect_fill(HX + 1, HY + 1, HW - 2, HH - 2, 32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(HX, HY, HW, HH, " VTIRC Quick Manual - MWheel to scroll ", VT_TUI_WIN_SHADOW)
        vt_tui_editor_draw(ED_X, ED_Y, ED_W, ED_H, ed_st)
        vt_tui_form_draw(hlp_items(), hlp_focused)
        vt_sleep(16)
    Loop
End Sub

' -----------------------------------------------------------------------------
' F2 -- Server settings form  (disconnect required)
' returns 1 = Connect pressed, 0 = Cancel / Escape
' -----------------------------------------------------------------------------
Function settings_server_form() As Long
    Const ITEM_COUNT = 12
    Dim items(0 To ITEM_COUNT - 1) As vt_tui_form_item
    Dim focused As Long = 1
    Dim k       As ULong
    Dim result  As Long

    ' labels at even indices 0,2,4,6,8
    Dim li As Long
    For li = 0 To 8 Step 2
        items(li).kind   = VT_FORM_LABEL
        items(li).wid    = 10
        items(li).align  = VT_ALIGN_RIGHT
        items(li).lbl_fg = VT_BLACK
        items(li).lbl_bg = VT_LIGHT_GREY
    Next li
    items(0).x = 1 : items(0).y = 1 : items(0).val = "Server:"
    items(2).x = 1 : items(2).y = 3 : items(2).val = "Port:"
    items(4).x = 1 : items(4).y = 5 : items(4).val = "Channel:"
    items(6).x = 1 : items(6).y = 7 : items(6).val = "Nick:"
    items(8).x = 1 : items(8).y = 9 : items(8).val = "Password:"

    items(1).kind    = VT_FORM_INPUT
    items(1).x       = 12 : items(1).y      = 1
    items(1).wid     = 30 : items(1).max_len = 63
    items(1).val     = cfg.server
    items(1).cpos    = Len(cfg.server)

    items(3).kind    = VT_FORM_INPUT
    items(3).x       = 12 : items(3).y      = 3
    items(3).wid     = 8  : items(3).max_len = 5
    items(3).val     = Trim(Str(cfg.port))
    items(3).cpos    = Len(items(3).val)

    items(5).kind    = VT_FORM_INPUT
    items(5).x       = 12 : items(5).y      = 5
    items(5).wid     = 30 : items(5).max_len = 50
    items(5).val     = cfg.channel
    items(5).cpos    = Len(cfg.channel)

    items(7).kind    = VT_FORM_INPUT
    items(7).x       = 12 : items(7).y      = 7
    items(7).wid     = 20 : items(7).max_len = 30
    items(7).val     = cfg.nick
    items(7).cpos    = Len(cfg.nick)

    items(9).kind    = VT_FORM_INPUT
    items(9).x       = 12 : items(9).y      = 9
    items(9).wid     = 30 : items(9).max_len = 64
    items(9).val     = cfg.password
    items(9).cpos    = Len(cfg.password)

    items(10).kind = VT_FORM_BUTTON
    items(10).x    = 10 : items(10).y = 11
    items(10).val  = "Connect"
    items(10).ret  = 1

    items(11).kind = VT_FORM_BUTTON
    items(11).x    = 25 : items(11).y = 11
    items(11).val  = "Cancel"
    items(11).ret  = 2

    vt_tui_form_offset(items(), FORM_X, FORM_Y)
    draw_all()

    Do
        k      = vt_inkey()
        result = vt_tui_form_handle(items(), focused, k)
        vt_tui_rect_fill(FORM_X + 1, FORM_Y + 1, FORM_W - 2, FORM_H - 2, _
                         32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(FORM_X, FORM_Y, FORM_W, FORM_H, " VTIRC Server Settings ", _
                      VT_TUI_WIN_SHADOW)
        vt_tui_form_draw(items(), focused)
        vt_sleep(16)

        Select Case result
        Case VT_FORM_CANCEL, 2
            Return 0
        Case 1
            cfg.server   = Trim(items(1).val)
            cfg.port     = Val(Trim(items(3).val))
            If cfg.port < 1 Or cfg.port > 65535 Then cfg.port = 6667
            cfg.channel  = Trim(items(5).val)
            cfg.nick     = Trim(items(7).val)
            cfg.password = items(9).val
            cfg_save()
            Return 1
        End Select
    Loop

    Return 0
End Function

' -----------------------------------------------------------------------------
' F3 -- Common settings form  (always accessible, no disconnect needed)
' returns 1 = OK pressed, 0 = Cancel / Escape
' -----------------------------------------------------------------------------
Function settings_common_form() As Long
    Const ITEM_COUNT = 12
    Dim items(0 To ITEM_COUNT - 1) As vt_tui_form_item
    Dim focused As Long = 1   ' skip label at 0, start on first radio
    Dim k       As ULong
    Dim result  As Long

    ' row 1: colour scheme header label (full inner width)
    items(0).kind   = VT_FORM_LABEL
    items(0).x      = 1 : items(0).y   = 1
    items(0).wid    = CFORM_W - 2
    items(0).val    = "Colour Scheme:"
    items(0).align  = VT_ALIGN_LEFT
    items(0).lbl_fg = VT_BLACK
    items(0).lbl_bg = VT_LIGHT_GREY

    ' row 3: scheme radios
    items(1).kind     = VT_FORM_RADIO
    items(1).x        = 2  : items(1).y = 3
    items(1).val      = "Dark"
    items(1).group_id = 1
    items(1).checked  = IIf(cfg.scheme = 0, 1, 0)

    items(2).kind     = VT_FORM_RADIO
    items(2).x        = 13 : items(2).y = 3
    items(2).val      = "Classic"
    items(2).group_id = 1
    items(2).checked  = IIf(cfg.scheme = 1, 1, 0)

    items(3).kind     = VT_FORM_RADIO
    items(3).x        = 25 : items(3).y = 3
    items(3).val      = "Light"
    items(3).group_id = 1
    items(3).checked  = IIf(cfg.scheme = 2, 1, 0)

    ' row 5: channel logging label + checkbox
    items(4).kind   = VT_FORM_LABEL
    items(4).x      = 1 : items(4).y   = 5
    items(4).wid    = 10
    items(4).val    = "Logging:"
    items(4).align  = VT_ALIGN_RIGHT
    items(4).lbl_fg = VT_BLACK
    items(4).lbl_bg = VT_LIGHT_GREY

    items(5).kind    = VT_FORM_CHECKBOX
    items(5).x       = 12 : items(5).y = 5
    items(5).val     = "Log channel"
    items(5).checked = cfg.log_enabled

    ' row 7: PM logging label + checkbox
    items(6).kind   = VT_FORM_LABEL
    items(6).x      = 1 : items(6).y   = 7
    items(6).wid    = 10
    items(6).val    = "PM Log:"
    items(6).align  = VT_ALIGN_RIGHT
    items(6).lbl_fg = VT_BLACK
    items(6).lbl_bg = VT_LIGHT_GREY

    items(7).kind    = VT_FORM_CHECKBOX
    items(7).x       = 12 : items(7).y = 7
    items(7).val     = "Log PMs"
    items(7).checked = cfg.log_pm

    ' row 9: network label + auto-reconnect checkbox
    items(8).kind   = VT_FORM_LABEL
    items(8).x      = 1 : items(8).y   = 9
    items(8).wid    = 10
    items(8).val    = "Network:"
    items(8).align  = VT_ALIGN_RIGHT
    items(8).lbl_fg = VT_BLACK
    items(8).lbl_bg = VT_LIGHT_GREY

    items(9).kind    = VT_FORM_CHECKBOX
    items(9).x       = 12 : items(9).y = 9
    items(9).val     = "Auto-reconnect"
    items(9).checked = cfg.auto_reconnect

    ' row 11: buttons
    items(10).kind = VT_FORM_BUTTON
    items(10).x    = 10 : items(10).y = 11
    items(10).val  = "OK"
    items(10).ret  = 1

    items(11).kind = VT_FORM_BUTTON
    items(11).x    = 22 : items(11).y = 11
    items(11).val  = "Cancel"
    items(11).ret  = 2

    vt_tui_form_offset(items(), CFORM_X, CFORM_Y)
    draw_all()

    Do
        irc_poll()
        k      = vt_inkey()
        result = vt_tui_form_handle(items(), focused, k)
        vt_tui_rect_fill(CFORM_X + 1, CFORM_Y + 1, CFORM_W - 2, CFORM_H - 2, _
                         32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(CFORM_X, CFORM_Y, CFORM_W, CFORM_H, " VTIRC Settings ", _
                      VT_TUI_WIN_SHADOW)
        vt_tui_form_draw(items(), focused)
        vt_sleep(16)

        Select Case result
        Case VT_FORM_CANCEL, 2
            Return 0
        Case 1
            ' apply colour scheme
            If items(1).checked Then cfg.scheme = 0
            If items(2).checked Then cfg.scheme = 1
            If items(3).checked Then cfg.scheme = 2
            ' handle channel log toggle with correct ordering so markers land in file
            Dim new_log As Byte = items(5).checked
            If cfg.log_enabled = 1 AndAlso new_log = 0 Then
                hist_append("*** Logging stopped.", col_fg_sys)
                cfg.log_enabled = 0
            ElseIf cfg.log_enabled = 0 AndAlso new_log = 1 Then
                cfg.log_enabled = 1
                hist_append("*** Logging started.", col_fg_sys)
            End If
            ' PM logging -- no start/stop marker needed, just toggle
            cfg.log_pm = items(7).checked
            ' auto-reconnect -- if turned off while a retry is pending, cancel it
            Dim new_ar As Byte = items(9).checked
            If cfg.auto_reconnect = 1 AndAlso new_ar = 0 Then
                reconnect_pending = 0
            End If
            cfg.auto_reconnect = new_ar
            cfg_save()
            scheme_apply()
            Return 1
        End Select
    Loop

    Return 0
End Function

' -----------------------------------------------------------------------------
' F4 -- User list TUI  (listbox + PM / Close buttons)
' -----------------------------------------------------------------------------
Sub user_list_form()
    If user_count = 0 Then
        vt_tui_dialog("User List", "No users tracked for " & cfg.channel & ".", VT_DLG_OK)
        Return
    End If

    Const UW  = 28
    Const UH  = 18
    Const UX  = (SCREEN_COLS - UW) \ 2   ' = 26
    Const UY  = (SCREEN_ROWS - UH) \ 2   ' = 3
    Const LBX = UX + 1                   ' listbox screen x
    Const LBY = UY + 1                   ' listbox screen y
    Const LBW = UW - 2                   ' = 26 cols wide
    Const LBH = UH - 5                   ' = 13 visible rows

    ' snapshot of user list -- indices stay stable while the form is open
    Dim lb_items(user_count - 1) As String
    Dim lb_st        As vt_tui_listbox_state
    Dim btn_items(1) As vt_tui_form_item
    Dim btn_focused  As Long = 0
    Dim lb_focused   As Byte = 1   ' 1 = listbox has kb focus, 0 = buttons
    Dim sel_nick     As String
    Dim k            As ULong
    Dim lb_ret       As Long
    Dim btn_ret      As Long
    Dim wi           As Long
    Dim ii           As Long

    For ii = 0 To user_count - 1
        lb_items(ii) = user_list(ii)
    Next ii

    lb_st.sel             = 0
    lb_st.top_item        = 0
    lb_st.last_click_item = -1
    lb_st.last_click_time = 0.0
    lb_st.prev_btns       = 0

    ' local coords, shifted to screen by vt_tui_form_offset:
    '   local (1,1) + (UX,UY) -> screen (UX+1, UY+1)
    '   button row: local y = UH-3 = 15 -> screen row UY+15 = 18
    btn_items(0).kind = VT_FORM_BUTTON
    btn_items(0).x    = 5
    btn_items(0).y    = UH - 3
    btn_items(0).val  = "PM"
    btn_items(0).ret  = 1

    btn_items(1).kind = VT_FORM_BUTTON
    btn_items(1).x    = 14
    btn_items(1).y    = UH - 3
    btn_items(1).val  = "Close"
    btn_items(1).ret  = 2

    vt_tui_form_offset(btn_items(), UX, UY)

    draw_all()   ' frozen background while modal is open

    Do
        irc_poll()
        k       = vt_inkey()
        lb_ret  = VT_FORM_PENDING
        btn_ret = VT_FORM_PENDING

        If VT_SCAN(k) = VT_KEY_ESC Then Exit Do

        ' Tab switches kb focus -- consumed here, neither widget sees it
        If VT_SCAN(k) = VT_KEY_TAB Then lb_focused Xor= 1 : k = 0

        ' Both widgets called every frame so mouse events always reach both.
        ' Keyboard (k) goes to whichever has focus; the other gets 0.
        If lb_focused Then
            lb_ret  = vt_tui_listbox_handle(LBX, LBY, LBW, LBH, lb_items(), lb_st, k)
            btn_ret = vt_tui_form_handle(btn_items(), btn_focused, 0, VT_FORM_NO_ESC)
        Else
            lb_ret  = vt_tui_listbox_handle(LBX, LBY, LBW, LBH, lb_items(), lb_st, 0)
            btn_ret = vt_tui_form_handle(btn_items(), btn_focused, k, VT_FORM_NO_ESC)
        End If

        ' Enter while listbox is focused = same as PM button
        If lb_focused AndAlso lb_ret >= 0 Then btn_ret = 1

        If btn_ret = 1 Then   ' PM
            sel_nick = lb_items(lb_st.sel)
            If LCase(sel_nick) = LCase(cfg.nick) Then
                ' cannot PM yourself -- just close the form
                Exit Do
            End If
            wi = win_open(sel_nick, 1)
            If wi >= 0 Then
                active_win      = wi
                wins(wi).unread = 0
            Else
                vt_tui_dialog("PM", "Too many windows open (max " & WIN_MAX & ").", VT_DLG_OK)
            End If
            Exit Do
        ElseIf btn_ret = 2 Then   ' Close
            Exit Do
        End If

        vt_tui_rect_fill(UX + 1, UY + 1, UW - 2, UH - 2, 32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(UX, UY, UW, UH, " Users (" & user_count & ") ", VT_TUI_WIN_SHADOW)
        vt_tui_listbox_draw(LBX, LBY, LBW, LBH, lb_items(), lb_st)
        vt_tui_form_draw(btn_items(), btn_focused)
        vt_sleep(16)
    Loop
End Sub

' -----------------------------------------------------------------------------
' IRC: send
' -----------------------------------------------------------------------------
Sub irc_send(irc_msg As String)
    If sock_valid = 0 Then Return
    ' IRC protocol: max 512 bytes total including CRLF, so cap message at 510
    If Len(irc_msg) > 510 Then irc_msg = Left(irc_msg, 510)
    Dim full_msg As String = irc_msg & Chr(13) & Chr(10)
    Dim snd_len  As Long   = Len(full_msg)
    Dim sent     As Long   = 0
    Dim nb       As Long
    Dim zbuf     As ZString * 513
    zbuf = full_msg
    ' loop until all bytes are sent (vt_net_send may return fewer than requested)
    Do While sent < snd_len
        nb = vt_net_send(sock, @zbuf + sent, snd_len - sent)
        If nb <= 0 Then Exit Do
        sent += nb
    Loop
End Sub

' IRC: parse raw line into prefix / command / params / trailing
Sub irc_parse(raw_ln As String, ByRef pfx As String, ByRef cmd As String, _
              ByRef prms As String, ByRef trail As String)
    pfx = "" : cmd = "" : prms = "" : trail = ""
    Dim rest As String = raw_ln
    Dim sp1  As Long
    Dim sp2  As Long
    Dim tp   As Long
    If Left(rest, 1) = ":" Then
        sp1 = InStr(rest, " ")
        If sp1 = 0 Then pfx = Mid(rest, 2) : Return
        pfx  = Mid(rest, 2, sp1 - 2)
        rest = Mid(rest, sp1 + 1)
    End If
    sp2 = InStr(rest, " ")
    If sp2 = 0 Then cmd = rest : Return
    cmd  = Left(rest, sp2 - 1)
    rest = Mid(rest, sp2 + 1)
    tp   = InStr(rest, " :")
    If tp > 0 Then
        trail = Mid(rest, tp + 2)
        prms  = Left(rest, tp - 1)
    ElseIf Left(rest, 1) = ":" Then
        trail = Mid(rest, 2)
    Else
        prms = rest
    End If
End Sub

Function nick_from_pfx(pfx As String) As String
    Dim ex As Long = InStr(pfx, "!")
    If ex > 0 Then Return Left(pfx, ex - 1)
    Return pfx
End Function

Function irc_strip_colors(s As String) As String
    Dim result As String
    Dim i      As Long  = 1
    Dim slen   As Long  = Len(s)
    Dim ch     As UByte
    Dim digits As Long
    Do While i <= slen
        ch = Asc(s, i)
        Select Case ch
        Case 3   ' Chr(3)[nn[,nn]] -- skip color code and its digit arguments
            i += 1
            digits = 0
            Do While i <= slen AndAlso digits < 2
                If Asc(s, i) >= Asc("0") AndAlso Asc(s, i) <= Asc("9") Then
                    i += 1 : digits += 1
                Else
                    Exit Do
                End If
            Loop
            If i <= slen AndAlso Asc(s, i) = Asc(",") Then
                i += 1
                digits = 0
                Do While i <= slen AndAlso digits < 2
                    If Asc(s, i) >= Asc("0") AndAlso Asc(s, i) <= Asc("9") Then
                        i += 1 : digits += 1
                    Else
                        Exit Do
                    End If
                Loop
            End If
        Case 2, 15, 22, 31   ' bold, reset, reverse video, underline
            i += 1
        Case Else
            result &= Chr(ch)
            i += 1
        End Select
    Loop
    Return result
End Function

' -----------------------------------------------------------------------------
' Color input: translate ^n (EGA index 0-15) to mIRC Chr(3)nn sequences
' Parsing is greedy: ^10 is EGA 10, not EGA 1 + "0".
' Chr(15) (mIRC reset) is appended when any color was injected.
' -----------------------------------------------------------------------------
Function q3_to_mirc(txt As String) As String
    ' EGA index -> mIRC color index (best-effort, mIRC has no bright-red)
    Static ega_to_mirc(15) As UByte = { _
        1, 2, 3, 10, 4, 6, 7, 15, 14, 12, 9, 11, 4, 13, 8, 0 }
    Dim result    As String
    Dim slen      As Long = Len(txt)
    Dim i         As Long = 1
    Dim has_color As Byte = 0
    Do While i <= slen
        If Asc(txt, i) = Asc("^") AndAlso i < slen Then
            Dim d1 As Long = Asc(txt, i + 1) - Asc("0")
            If d1 >= 0 AndAlso d1 <= 9 Then
                Dim clr_idx As Long = d1
                Dim skip    As Long = 1
                ' try two-digit index (10-15)
                If i + 2 <= slen Then
                    Dim d2   As Long = Asc(txt, i + 2) - Asc("0")
                    Dim try2 As Long = d1 * 10 + d2
                    If d2 >= 0 AndAlso d2 <= 9 AndAlso try2 <= 15 Then
                        clr_idx = try2
                        skip    = 2
                    End If
                End If
                result    &= Chr(3) & Right("0" & ega_to_mirc(clr_idx), 2)
                has_color  = 1
                i         += 1 + skip
                Continue Do
            End If
        End If
        result &= Chr(Asc(txt, i))
        i += 1
    Loop
    If has_color Then result &= Chr(15)   ' reset to plain at end of message
    Return result
End Function

' -----------------------------------------------------------------------------
' User list  (populated from NAMES reply, kept live via JOIN/PART/QUIT/NICK)
' -----------------------------------------------------------------------------
Sub user_list_clear()
    user_count = 0
End Sub

Sub user_list_add(nick As String)
    ' strip mode-prefix chars that the server may prepend (@+%&~)
    Dim n As String = nick
    Do While Len(n) > 0
        Select Case Left(n, 1)
        Case "@", "+", "%", "&", "~"
            n = Mid(n, 2)
        Case Else
            Exit Do
        End Select
    Loop
    If Len(n) = 0 Then Return
    ' ignore duplicate (case-insensitive)
    Dim i As Long
    For i = 0 To user_count - 1
        If LCase(user_list(i)) = LCase(n) Then Return
    Next i
    If user_count >= USER_MAX Then Return
    user_list(user_count) = n
    user_count += 1
End Sub

Sub user_list_remove(nick As String)
    Dim i As Long
    For i = 0 To user_count - 1
        If LCase(user_list(i)) = LCase(nick) Then
            Dim j As Long
            For j = i To user_count - 2
                user_list(j) = user_list(j + 1)
            Next j
            user_list(user_count - 1) = ""
            user_count -= 1
            Return
        End If
    Next i
End Sub

Sub user_list_rename(old_nick As String, new_nick As String)
    Dim i As Long
    For i = 0 To user_count - 1
        If LCase(user_list(i)) = LCase(old_nick) Then
            user_list(i) = new_nick
            Return
        End If
    Next i
End Sub

Sub user_list_echo()
    If user_count = 0 Then
        hist_append("*** No users tracked for " & cfg.channel, col_fg_sys)
        Return
    End If
    hist_append("*** " & user_count & " users in " & cfg.channel & ":", col_fg_sys)
    Dim ln As String
    Dim i  As Long
    For i = 0 To user_count - 1
        Dim entry As String = user_list(i)
        If Len(ln) + 1 + Len(entry) > SCREEN_COLS - 4 Then
            hist_append("  " & ln, col_fg_sys)
            ln = entry
        Else
            If Len(ln) > 0 Then ln &= " "
            ln &= entry
        End If
    Next i
    If Len(ln) > 0 Then hist_append("  " & ln, col_fg_sys)
End Sub

' -----------------------------------------------------------------------------
' Window array helpers
' -----------------------------------------------------------------------------
Function win_find(tgt As String) As Long
    Dim i As Long
    For i = 0 To win_count - 1
        If LCase(wins(i).target) = LCase(tgt) Then Return i
    Next i
    Return -1
End Function

Function win_open(tgt As String, pm As Byte) As Long
    Dim idx As Long = win_find(tgt)
    If idx >= 0 Then Return idx
    If win_count >= WIN_MAX Then Return -1
    idx = win_count
    wins(idx).target     = tgt
    wins(idx).is_pm      = pm
    wins(idx).unread     = 0
    wins(idx).hist_count = 0
    wins(idx).hist_head  = 0
    wins(idx).top_line   = 0
    wins(idx).new_msgs   = 0
    win_count += 1
    Return idx
End Function

' Remove a PM window by shifting the array left.
' Cannot close window 0 (the channel).
' active_win is clamped so it never points past the new end.
Sub win_close_pm(win_idx As Long)
    If win_idx <= 0 OrElse win_idx >= win_count Then Exit Sub
    Dim i As Long
    For i = win_idx To win_count - 2
        wins(i) = wins(i + 1)
    Next i
    ' clear the vacated last slot
    wins(win_count - 1).target     = ""
    wins(win_count - 1).is_pm      = 0
    wins(win_count - 1).unread     = 0
    wins(win_count - 1).hist_count = 0
    wins(win_count - 1).hist_head  = 0
    wins(win_count - 1).top_line   = 0
    wins(win_count - 1).new_msgs   = 0
    win_count -= 1
    If active_win >= win_count Then active_win = win_count - 1
End Sub

' -----------------------------------------------------------------------------
' IRC: handle one complete server line
' -----------------------------------------------------------------------------
Sub irc_handle(raw_ln As String)
    Dim pfx_str   As String
    Dim cmd_str   As String
    Dim prms_str  As String
    Dim trail_str As String
    irc_parse(raw_ln, pfx_str, cmd_str, prms_str, trail_str)

    Dim src_nick As String = nick_from_pfx(pfx_str)
    Dim cmd_up   As String = UCase(cmd_str)

    Select Case cmd_up
    Case "PING"
        irc_send("PONG :" & trail_str)

    Case "PRIVMSG"
        Dim priv_tgt As String = LCase(Trim(prms_str))
        If priv_tgt = LCase(cfg.channel) Then
            ' channel message -> always window 0
            Dim msg_fg As UByte
            If LCase(src_nick) = LCase(cfg.nick) Then
                msg_fg = col_fg_own
            Else
                msg_fg = nick_color(src_nick)
            End If
            win_hist_append(0, "<" & src_nick & "> " & trail_str, msg_fg)
        ElseIf priv_tgt = LCase(cfg.nick) Then
            ' PM directed at us -- find or create a window for src_nick
            Dim pm_wi As Long = win_open(src_nick, 1)
            If pm_wi >= 0 Then
                win_hist_append(pm_wi, "<" & src_nick & "> " & trail_str, nick_color(src_nick))
            End If
        End If

    Case "NOTICE"
        Dim notice_src  As String = src_nick
        Dim notice_tgt  As String = Trim(prms_str)
        Dim notice_txt  As String = irc_strip_colors(trail_str)
        Dim notice_line As String
        If LCase(notice_tgt) = LCase(cfg.channel) Then
            notice_line = "-" & notice_src & ":" & notice_tgt & "- " & notice_txt
        Else
            notice_line = "-" & notice_src & "- " & notice_txt
        End If
        hist_append(notice_line, VT_YELLOW)

    Case "JOIN"
        Dim jch As String = trail_str
        If jch = "" Then jch = prms_str
        hist_append("*** " & src_nick & " has joined " & jch, col_fg_sys)
        If LCase(jch) = LCase(cfg.channel) Then user_list_add(src_nick)

    Case "PART"
        hist_append("*** " & src_nick & " has left " & cfg.channel, col_fg_sys)
        user_list_remove(src_nick)

    Case "QUIT"
        hist_append("*** " & src_nick & " has quit (" & irc_strip_colors(trail_str) & ")", col_fg_sys)
        user_list_remove(src_nick)

    Case "NICK"
        Dim new_nick As String = irc_strip_colors(trail_str)
        If LCase(src_nick) = LCase(cfg.nick) Then
            cfg.nick = new_nick
            hist_append("*** You are now known as " & new_nick, col_fg_sys)
        Else
            hist_append("*** " & src_nick & " is now known as " & new_nick, col_fg_sys)
        End If
        user_list_rename(src_nick, new_nick)
        ' propagate nick rename into any open PM window targeting the old nick
        Dim wn As Long
        For wn = 1 To win_count - 1
            If wins(wn).is_pm AndAlso LCase(wins(wn).target) = LCase(src_nick) Then
                wins(wn).target = new_nick
                win_hist_append(wn, "*** " & src_nick & " is now known as " & new_nick, col_fg_sys)
            End If
        Next wn

    Case "001"
        hist_append("*** " & irc_strip_colors(trail_str), col_fg_sys)
        connected = 1
        wins(0).target = cfg.channel
        wins(0).is_pm  = 0
        user_list_clear()
        names_receiving = 0
        irc_send("JOIN " & cfg.channel)

    Case "305"   ' RPL_UNAWAY
        is_afk  = 0
        afk_msg = ""
        hist_append("*** You are no longer marked as away.", col_fg_sys)

    Case "306"   ' RPL_NOWAWAY
        hist_append("*** You are now marked as away.", col_fg_sys)

    Case "353"   ' RPL_NAMREPLY -- one of potentially several nick-list chunks
        If names_receiving = 0 Then
            user_list_clear()
            names_receiving = 1
        End If
        Dim nk_parts() As String
        Dim nk_count   As Long = vt_str_split(trail_str, " ", nk_parts())
        Dim ni         As Long
        For ni = 0 To nk_count - 1
            If Len(nk_parts(ni)) > 0 Then user_list_add(nk_parts(ni))
        Next ni

    Case "366"   ' RPL_ENDOFNAMES -- batch complete
        names_receiving = 0
        hist_append("*** " & user_count & " users in " & cfg.channel, col_fg_sys)

    Case "433"
        hist_append("*** Nickname in use: try /nick <newnick>", VT_BRIGHT_RED)

    Case Else
        Dim cmd_num As Long = Val(cmd_up)
        If cmd_num >= 1 AndAlso cmd_num <= 999 Then
            Select Case cmd_num
            Case 372, 375, 376   ' MOTD body/start/end -- suppress
            Case Else
                If Len(trail_str) > 0 Then
                    hist_append("  " & irc_strip_colors(trail_str), VT_DARK_GREY)
                End If
            End Select
        End If
    End Select
End Sub

' -----------------------------------------------------------------------------
' IRC: disconnect
' -----------------------------------------------------------------------------
Sub irc_disconnect()
    If sock_valid = 0 Then Return
    reconnect_pending = 0   ' user-initiated -- suppress any pending auto-reconnect
    irc_send("QUIT :VTIRC")
    vt_net_close(sock)
    vt_net_shutdown()
    sock_valid = 0
    connected  = 0
    recv_buf   = ""
    ' drop all PM windows -- channel history in wins(0) stays visible
    win_count  = 1
    active_win = 0
    wins(0).unread = 0
    hist_append("*** Disconnected from " & cfg.server, col_fg_sys)
End Sub

' -----------------------------------------------------------------------------
' IRC: poll incoming data (call each frame when connected)
' -----------------------------------------------------------------------------
Sub irc_poll()
    If sock_valid = 0 Then Return
    Dim tmp_buf  As ZString * 4097
    Dim nb       As Long
    Dim crlf_pos As Long
    ' drain all pending data in one call -- loop until the socket has nothing left
    Do While vt_net_ready(sock, 0, 0) = 1
        nb = vt_net_recv(sock, @tmp_buf, 4096)
        If nb <= 0 Then
            irc_on_drop()
            Return
        End If
        recv_buf &= Left(tmp_buf, nb)
        ' safety cap: discard buffer if no CRLF arrives within a sane limit
        If Len(recv_buf) > 8192 Then recv_buf = ""
    Loop
    Do
        crlf_pos = InStr(recv_buf, Chr(13) & Chr(10))
        If crlf_pos = 0 Then Exit Do
        Dim raw_line As String = Left(recv_buf, crlf_pos - 1)
        recv_buf = Mid(recv_buf, crlf_pos + 2)
        If Len(raw_line) > 0 Then irc_handle(raw_line)
    Loop
End Sub

' -----------------------------------------------------------------------------
' IRC: handle unexpected server drop (peer closed or recv error)
' Called from irc_poll.  User-initiated disconnect goes through irc_disconnect
' and must NOT reach this path.
' -----------------------------------------------------------------------------
Sub irc_on_drop()
    vt_net_close(sock)
    vt_net_shutdown()
    sock_valid = 0
    connected  = 0
    recv_buf   = ""
    ' window array / history are intentionally kept intact for reconnect
    If cfg.auto_reconnect Then
        reconnect_pending = 1
        reconnect_at      = Timer + RECONNECT_DELAY
        hist_append("*** Connection lost. Reconnecting in " & _
                    RECONNECT_DELAY & "s...", VT_BRIGHT_RED)
    Else
        hist_append("*** Connection closed by server.", col_fg_sys)
    End If
End Sub

' -----------------------------------------------------------------------------
' IRC: connect to server from cfg
' -----------------------------------------------------------------------------
Function irc_connect() As Byte
    If Len(cfg.nick) = 0 OrElse Len(cfg.server) = 0 Then
        hist_append("*** No server or nick configured", VT_BRIGHT_RED)
        Return 0
    End If

    hist_append("*** Connecting to " & cfg.server & ":" & cfg.port & "...", col_fg_sys)
    draw_all()
    vt_present()

    If vt_net_init() <> 0 Then
        hist_append("*** Network init failed", VT_BRIGHT_RED)
        Return 0
    End If

    Dim srv_ip As Long = vt_net_resolve(StrPtr(cfg.server))
    If srv_ip = 0 Then
        hist_append("*** Could not resolve: " & cfg.server, VT_BRIGHT_RED)
        vt_net_shutdown()
        Return 0
    End If

    sock = vt_net_open()
    If sock = INVALID_SOCKET Then
        hist_append("*** Could not open socket", VT_BRIGHT_RED)
        vt_net_shutdown()
        Return 0
    End If

    If vt_net_connect(sock, srv_ip, cfg.port) = 0 Then
        hist_append("*** Connection refused: " & cfg.server, VT_BRIGHT_RED)
        vt_net_close(sock)
        vt_net_shutdown()
        Return 0
    End If

    sock_valid = 1
    wins(0).target = cfg.channel
    wins(0).is_pm  = 0
    wins(0).unread = 0
    ' on auto-reconnect keep history so messages append onto existing buffer
    If is_reconnect = 0 Then
        wins(0).hist_count = 0
        wins(0).hist_head  = 0
    End If
    is_reconnect       = 0
    wins(0).top_line   = 0
    wins(0).new_msgs   = 0
    win_count          = 1
    active_win         = 0
    vt_net_nonblocking(sock, 1)

    If Len(cfg.password) > 0 Then irc_send("PASS " & cfg.password)
    irc_send("NICK " & cfg.nick)
    irc_send("USER " & cfg.nick & " 0 * :VTIRC")
    ' JOIN is sent in irc_handle when 001 welcome arrives

    Return 1
End Function

' -----------------------------------------------------------------------------
' Main
' -----------------------------------------------------------------------------

vt_title("VTIRC " & VERSION)
'If vt_screen(VT_SCREEN_0, VT_WINDOWED Or VT_NO_RESIZE) <> 0 Then End 1
If vt_screen(VT_SCREEN_100_40, VT_WINDOWED) <> 0 Then End 1
vt_scroll_enable(0)
vt_mouse(1)
vt_on_close(@on_close)
vt_copypaste(VT_ENABLED)

cfg_file = ExePath() & "/.vtirc"
cfg_defaults()
cfg_load()
scheme_apply()   ' initialise colours before anything draws or appends

' initialise the shared input form item -- x/y/wid are set each frame in draw_input
input_form(0).kind    = VT_FORM_INPUT
input_form(0).max_len = 510
input_form(0).val     = ""
input_form(0).cpos    = 0
input_focused         = 0

If vt_file_exists(cfg_file) = 0 Then
    If settings_server_form() = 1 Then irc_connect()
Else
    irc_connect()
End If

' -- main loop ----------------------------------------------------------------

Dim k       As ULong
Dim max_scr As Long
Dim whl     As Long

Do
    ' -- auto-reconnect tick (runs every frame, costs nothing when idle) ------
    If reconnect_pending AndAlso Timer >= reconnect_at Then
        reconnect_pending = 0
        hist_append("*** Reconnecting...", col_fg_sys)
        is_reconnect = 1
        If irc_connect() = 0 AndAlso cfg.auto_reconnect Then
            reconnect_pending = 1
            reconnect_at      = Timer + RECONNECT_DELAY
            hist_append("*** Reconnect failed. Retrying in " & _
                        RECONNECT_DELAY & "s...", VT_BRIGHT_RED)
        End If
    End If

    k = vt_inkey()
    
    ' -- Ctrl+Tab: cycle windows  Ctrl+W: close active PM window --------------
    If VT_SCAN(k) = VT_KEY_TAB AndAlso VT_CTRL(k) Then
        If win_count > 1 Then
            active_win = (active_win + 1) Mod win_count
            wins(active_win).unread = 0
        End If
    ElseIf VT_CHAR(k) = 23 Then
        If active_win > 0 Then
            win_close_pm(active_win)
            wins(active_win).unread = 0
        End If
    Else
        ' -- input form: pass all keys except history navigation keys -----------
        Select Case VT_SCAN(k)
        Case VT_KEY_PGUP, VT_KEY_PGDN, VT_KEY_HOME, VT_KEY_END
            ' history scroll keys -- do not pass to input form
        Case Else
            vt_tui_form_handle(input_form(), input_focused, k, VT_FORM_NO_ESC)
        End Select
    End If

    ' -- mouse wheel scroll -- per active window -------------------------------
    vt_getmouse(0, 0, 0, @whl)
    If whl <> 0 Then
        max_scr = wins(active_win).hist_count - CHAT_ROWS
        If whl > 0 Then
            ' wheel up -- scroll back into history
            If max_scr > 0 AndAlso wins(active_win).top_line < max_scr Then
                wins(active_win).top_line += whl
                If wins(active_win).top_line > max_scr Then wins(active_win).top_line = max_scr
            End If
        Else
            ' wheel down -- scroll toward live
            wins(active_win).top_line += whl
            If wins(active_win).top_line < 0 Then wins(active_win).top_line = 0
            If wins(active_win).top_line = 0   Then wins(active_win).new_msgs = 0
        End If
    End If

    ' -- scroll keys, function keys, enter ------------------------------------
    Select Case VT_SCAN(k)
    Case VT_KEY_PGUP
        max_scr = wins(active_win).hist_count - CHAT_ROWS
        If max_scr > 0 AndAlso wins(active_win).top_line < max_scr Then
            wins(active_win).top_line += 3
            If wins(active_win).top_line > max_scr Then wins(active_win).top_line = max_scr
        End If
    Case VT_KEY_PGDN
        If wins(active_win).top_line > 3 Then
            wins(active_win).top_line -= 3
        Else
            wins(active_win).top_line = 0
            wins(active_win).new_msgs = 0
        End If
    Case VT_KEY_END
        wins(active_win).top_line = 0
        wins(active_win).new_msgs = 0
    Case VT_KEY_HOME
        max_scr = wins(active_win).hist_count - CHAT_ROWS
        If max_scr > 0 Then wins(active_win).top_line = max_scr

    ' -- function keys --------------------------------------------------------
    Case VT_KEY_F1
        help_window()
    Case VT_KEY_F2
        If connected Then
            vt_tui_dialog("Settings", "Disconnect first before changing server settings.", VT_DLG_OK)
        Else
            If settings_server_form() = 1 Then irc_connect()
        End If
    Case VT_KEY_F3
        settings_common_form()
    Case VT_KEY_F4
        If connected Then user_list_form()
    Case VT_KEY_F10
        If sock_valid Then irc_disconnect()

    ' -- send on Enter --------------------------------------------------------
    Case VT_KEY_ENTER
        If Len(input_form(0).val) > 0 Then
            If Left(input_form(0).val, 1) = "/" Then
                ' slash command
                Dim cmd_raw  As String = Mid(input_form(0).val, 2)
                Dim sp_pos   As Long   = InStr(cmd_raw, " ")
                Dim cmd_word As String
                Dim cmd_arg  As String
                If sp_pos > 0 Then
                    cmd_word = UCase(Left(cmd_raw, sp_pos - 1))
                    cmd_arg  = Mid(cmd_raw, sp_pos + 1)
                Else
                    cmd_word = UCase(cmd_raw)
                    cmd_arg  = ""
                End If

                Select Case cmd_word
                Case "QUIT"
                    irc_disconnect()

                Case "NICK"
                    If Len(cmd_arg) > 0 Then irc_send("NICK " & cmd_arg)

                Case "JOIN"
                    If Len(cmd_arg) > 0 Then
                        irc_send("JOIN " & cmd_arg)
                        cfg.channel    = cmd_arg
                        wins(0).target = cmd_arg
                        user_list_clear()
                        names_receiving = 0
                    End If

                Case "PART"
                    irc_send("PART " & cfg.channel)
                    hist_append("*** You left " & cfg.channel, col_fg_sys)

                Case "AFK"
                    If connected Then
                        If Len(cmd_arg) = 0 Then cmd_arg = "AFK"
                        irc_send("AWAY :" & cmd_arg)
                        is_afk  = 1
                        afk_msg = cmd_arg
                    Else
                        hist_append("*** Not connected.", VT_BRIGHT_RED)
                    End If

                Case "BACK"
                    If connected Then
                        irc_send("AWAY")
                        is_afk  = 0
                        afk_msg = ""
                    Else
                        hist_append("*** Not connected.", VT_BRIGHT_RED)
                    End If

                Case "NAMES"
                    user_list_echo()

                Case "MSG"
                    ' /msg <nick> [initial message]
                    If connected AndAlso Len(cmd_arg) > 0 Then
                        Dim msg_sp  As Long   = InStr(cmd_arg, " ")
                        Dim msg_tgt As String
                        Dim msg_txt As String
                        If msg_sp > 0 Then
                            msg_tgt = Left(cmd_arg, msg_sp - 1)
                            msg_txt = Mid(cmd_arg, msg_sp + 1)
                        Else
                            msg_tgt = cmd_arg
                            msg_txt = ""
                        End If
                        If LCase(msg_tgt) = LCase(cfg.nick) Then
                            hist_append("*** Cannot open a PM with yourself.", VT_BRIGHT_RED)
                        Else
                            Dim msg_wi As Long = win_open(msg_tgt, 1)
                            If msg_wi >= 0 Then
                                active_win              = msg_wi
                                wins(active_win).unread = 0
                                If Len(msg_txt) > 0 Then
                                    Dim msg_wire As String = q3_to_mirc(msg_txt)
                                    irc_send("PRIVMSG " & msg_tgt & " :" & msg_wire)
                                    win_hist_append(msg_wi, "<" & cfg.nick & "> " & msg_wire, col_fg_own)
                                End If
                            Else
                                hist_append("*** Too many windows open (max " & WIN_MAX & ")", VT_BRIGHT_RED)
                            End If
                        End If
                    End If

                Case Else
                    hist_append("*** Unknown command: /" & cmd_word, VT_BRIGHT_RED)
                End Select
            ElseIf connected Then
                ' route outgoing message to the active window's target
                Dim wire_txt As String = q3_to_mirc(input_form(0).val)
                irc_send("PRIVMSG " & wins(active_win).target & " :" & wire_txt)
                win_hist_append(active_win, "<" & cfg.nick & "> " & wire_txt, col_fg_own)
                wins(active_win).top_line = 0
                wins(active_win).new_msgs = 0
            End If
            ' clear input field
            input_form(0).val      = ""
            input_form(0).cpos     = 0
            input_form(0).view_off = 0
        End If
    End Select

    If VT_SCAN(k) = VT_KEY_F4 AndAlso VT_ALT(k) Then quit_flag = 1

    ' -- network poll ---------------------------------------------------------
    irc_poll()

    draw_all()
    vt_sleep(16)
Loop Until quit_flag

If sock_valid Then irc_disconnect()
vt_shutdown()
