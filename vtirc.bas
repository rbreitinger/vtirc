' vtirc.bas -- VTIRC minimal IRC client built on libvt
' FreeBASIC 1.10.1 | Windows + Linux
#cmdline "-s gui -gen gcc -O 2"
#Define VT_USE_NET
#Define VT_USE_SORT
#Define VT_USE_TUI
#Include Once "vt/vt.bi"

Const VERSION      = "1.5.0"
Const IDLE_MS      = 25
Const HISTORY_MAX  = 2000
Const USER_MAX     = 512
Const CHAT_TOP_ROW = 2
Const CHAT_BOT_ROW = 37
Const CHAT_ROWS    = 36
Const ROW_INPUT    = 39
Const ROW_STATUS   = 40
Const SCREEN_COLS  = 100
Const SCREEN_ROWS  = 40

' left user-list pane
Const PANE_W       = 18             ' pane width in columns
Const PANE_SEP     = PANE_W + 1     ' = 19, vertical separator column
Const CHAT_COL     = PANE_W + 2     ' = 20, first chat column
Const CHAT_WIDE    = SCREEN_COLS - PANE_W - 1  ' = 81, chat display width

' F2 server settings form
Const FORM_W       = 46
Const FORM_H       = 13
Const FORM_X       = (SCREEN_COLS - FORM_W) \ 2
Const FORM_Y       = (SCREEN_ROWS - FORM_H) \ 2

' F3 common settings form
Const CFORM_W      = 40
Const CFORM_H      = 15     ' +2 for "Show timestamps" checkbox row
Const LOG_TAIL_N   = 200    ' lines to replay from log on connect
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
    auto_reconnect  As Byte     ' 0=off  1=reconnect on unexpected drop
    show_timestamps As Byte     ' 0=off  1=show [HH:MM] prefix in chat history (default=on)
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
    user_list(USER_MAX - 1)  As String    ' per-channel user list
    user_count               As Long      ' number of tracked users
    names_receiving          As Byte      ' 1 while collecting 353 replies
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
Const RECONNECT_DELAY = 5.0
Dim Shared reconnect_pending As Byte   = 0
Dim Shared reconnect_at      As Double = 0
Dim Shared is_reconnect      As Byte   = 0

' active colour scheme
Dim Shared As Ubyte col_bg_main, col_fg_body, col_fg_own, col_fg_other, _
                    col_fg_sys, col_bar_fg, col_bar_bg, col_fg_link
                    
' URL hit-map: rebuilt every draw_history call, one slot per visible chat row.
Type url_hit_t
    col_start As Long   ' screen col where URL starts (1-based)
    col_end   As Long   ' screen col where URL ends (inclusive)
    url_str   As String ' plain URL text; empty = no URL on this row
End Type
Dim Shared url_hit_map(CHAT_ROWS - 1) As url_hit_t

' pane user-list state
Dim Shared pane_lb_st    As vt_tui_listbox_state
ReDim Shared pane_items(0) As String   ' rebuilt on pane_dirty; sorted alphabetically
Dim Shared pane_dirty    As Byte = 1

' channel/pm windows
Dim Shared wins(WIN_MAX - 1) As irc_window
Dim Shared win_count         As Long = 1
Dim Shared active_win        As Long = 0

' command-line history
Const CMD_HIST_MAX   = 32
Dim Shared cmd_hist_buf(CMD_HIST_MAX - 1) As String
Dim Shared cmd_hist_count As Long = 0
Dim Shared cmd_hist_head  As Long = 0   ' ring head (oldest entry index)
Dim Shared cmd_hist_pos   As Long = -1  ' -1 = not browsing

' channel browser: LIST collection state
Const CHLIST_MAX         = 500
Dim Shared chlist_active As Byte = 0   ' 1 while LIST is in progress
Dim Shared chlist_done   As Byte = 0   ' 1 when 323 (end of list) arrives
Dim Shared chlist_count  As Long = 0
ReDim Shared chlist_names(CHLIST_MAX - 1)  As String
ReDim Shared chlist_users(CHLIST_MAX - 1)  As Long
ReDim Shared chlist_topics(CHLIST_MAX - 1) As String

Declare Sub irc_poll()
Declare Sub irc_on_drop()
Declare Function win_find(tgt As String) As Long
Declare Function win_open(tgt As String, pm As Byte) As Long
Declare Sub win_hist_append(win_idx As Long, txt As String, col_fg As UByte)
Declare Sub win_hist_raw(win_idx As Long, txt As String, col_fg As UByte)
Declare Sub win_close(win_idx As Long)
Declare Function user_in_win(win_idx As Long, nick As String) As Byte
Declare Sub log_load_tail(win_idx As Long, n As Long)
Declare Sub pane_refresh()
Declare Sub user_list_clear(win_idx As Long)
Declare Sub user_list_add(win_idx As Long, nick As String)
Declare Sub user_list_remove(win_idx As Long, nick As String)
Declare Sub user_list_rename(win_idx As Long, old_nick As String, new_nick As String)
Declare Sub user_list_echo(win_idx As Long)
Declare Function irc_strip_colors(s As String) As String
Declare Function mirc_wordwrap(txt As String, wid As Long) As String
Declare Sub channel_browser()
Declare Function url_find_in_plain(plain_txt As String, ByRef u_start As Long, ByRef u_end As Long) As Byte
Declare Sub  open_url(url_txt As String)

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
        col_fg_sys   = VT_GREEN      : col_bar_fg   = VT_BRIGHT_MAGENTA : col_bar_bg = VT_BLUE
        col_fg_link  = VT_BRIGHT_CYAN

    Case 2  ' Light
        col_bg_main  = VT_LIGHT_GREY : col_fg_body  = VT_BLACK
        col_fg_own   = VT_BLUE       : col_fg_other = VT_RED
        col_fg_sys   = VT_DARK_GREY  : col_bar_fg   = VT_BLACK  : col_bar_bg = VT_WHITE
        col_fg_link  = VT_BLUE

    Case Else  ' Dark (0, default)
        col_bg_main  = VT_BLACK      : col_fg_body  = VT_LIGHT_GREY
        col_fg_own   = VT_WHITE      : col_fg_other = VT_BRIGHT_CYAN
        col_fg_sys   = VT_DARK_GREY  : col_bar_fg   = VT_CYAN  : col_bar_bg = VT_DARK_GREY
        col_fg_link  = VT_BRIGHT_CYAN

    End Select

    vt_tui_theme (col_fg_body  , col_bg_main, _    ' window body 
                  VT_WHITE  , VT_BLUE, _           ' title bar
                  col_bar_fg, col_bar_bg, _        ' bar/menu
                  VT_BLACK  , VT_LIGHT_GREY, _     ' button
                  VT_BLACK  , VT_LIGHT_GREY, _     ' dialogue
                  col_fg_body, col_bg_main )       ' input field
                  
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

' Logging
Sub log_write(ch_target As String, txt As String)
    If cfg.log_enabled = 0 Then Exit Sub
    If Len(cfg.server) = 0 OrElse Len(ch_target) = 0 Then Exit Sub
    Dim ch_safe  As String = ch_target
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
' -----------------------------------------------------------------------------
Sub win_hist_append(win_idx As Long, txt As String, col_fg As UByte)
    If win_idx < 0 OrElse win_idx >= win_count Then Exit Sub
    If wins(win_idx).is_pm = 0 Then
        If cfg.log_enabled Then log_write(wins(win_idx).target, irc_strip_colors(txt))
    ElseIf cfg.log_pm AndAlso wins(win_idx).is_pm Then
        log_write_pm(wins(win_idx).target, irc_strip_colors(txt))
    End If
    Dim disp_txt As String = txt
    If cfg.show_timestamps Then disp_txt = "[" & Left(Time(), 5) & "] " & txt
    Dim wrapped    As String = mirc_wordwrap(disp_txt, CHAT_WIDE)
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
    If win_idx <> active_win Then
        wins(win_idx).unread = 1
    Else
        If wins(win_idx).top_line > 0 Then wins(win_idx).new_msgs = 1
    End If
End Sub

Sub hist_append(txt As String, col_fg As UByte)
    If win_count = 0 Then Return
    win_hist_append(0, txt, col_fg)
End Sub

' -----------------------------------------------------------------------------
' win_hist_raw -- insert into ring buffer without logging or timestamp prefix.
' Used by log_load_tail so replayed lines are not re-logged and already carry
' their own [date time] prefix from the log file.
' -----------------------------------------------------------------------------
Sub win_hist_raw(win_idx As Long, txt As String, col_fg As UByte)
    If win_idx < 0 OrElse win_idx >= win_count Then Exit Sub
    Dim wrapped    As String = mirc_wordwrap(txt, CHAT_WIDE)
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
End Sub

' -----------------------------------------------------------------------------
' log_load_tail -- read last n lines from channel log into win 0 on connect.
' Only runs when log_enabled=1 (log was being written) and the file exists.
' Lines are inserted raw (no timestamp re-added, no re-logging).
' -----------------------------------------------------------------------------
Sub log_load_tail(win_idx As Long, n As Long)
    If cfg.log_enabled = 0 Then Exit Sub
    If win_idx < 0 OrElse win_idx >= win_count Then Exit Sub
    If wins(win_idx).is_pm Then Exit Sub
    If Len(cfg.server) = 0 OrElse Len(wins(win_idx).target) = 0 Then Exit Sub
    Dim ch_safe  As String = wins(win_idx).target
    If Left(ch_safe, 1) = "#" Then ch_safe = Mid(ch_safe, 2)
    Dim log_path As String = ExePath() & "/" & cfg.server & "_" & ch_safe & ".log"
    If vt_file_exists(log_path) = 0 Then Exit Sub

    ' Ring buffer to capture last n lines without loading full file into memory
    ReDim tail_lines(n - 1) As String
    Dim ring_head As Long = 0
    Dim ring_cnt  As Long = 0
    Dim f         As Long = FreeFile()
    Dim ln        As String
    Open log_path For Input As #f
    Do While EOF(f) = 0
        Line Input #f, ln
        If Len(ln) > 0 Then
            tail_lines(ring_head) = ln
            ring_head = (ring_head + 1) Mod n
            If ring_cnt < n Then ring_cnt += 1
        End If
    Loop
    Close #f

    If ring_cnt = 0 Then Exit Sub

    Dim ring_start As Long = IIf(ring_cnt < n, 0, ring_head)
    win_hist_raw(win_idx, "--- Replaying last " & ring_cnt & " log lines ---", col_fg_sys)
    Dim i As Long
    For i = 0 To ring_cnt - 1
        win_hist_raw(win_idx, tail_lines((ring_start + i) Mod n), VT_DARK_GREY)
    Next i
    win_hist_raw(win_idx, "--- End of log ---", col_fg_sys)
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
    cfg.auto_reconnect  = 0
    cfg.show_timestamps = 1
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
        Case "server"          : cfg.server          = cfg_val
        Case "port"            : cfg.port            = Val(cfg_val)
        Case "channel"         : cfg.channel         = cfg_val
        Case "nick"            : cfg.nick            = cfg_val
        Case "password"        : cfg.password        = cfg_val
        Case "scheme"          : cfg.scheme          = Val(cfg_val)
        Case "log_enabled"     : cfg.log_enabled     = Val(cfg_val)
        Case "log_pm"          : cfg.log_pm          = Val(cfg_val)
        Case "auto_reconnect"  : cfg.auto_reconnect  = Val(cfg_val)
        Case "show_timestamps" : cfg.show_timestamps = Val(cfg_val)
        End Select
    Loop
    Close #f
    Return 1
End Function

Sub cfg_save()
    Dim f As Long = FreeFile()
    Open cfg_file For Output As #f
    Print #f, "server="          & cfg.server
    Print #f, "port="            & cfg.port
    Print #f, "channel="         & cfg.channel
    Print #f, "nick="            & cfg.nick
    Print #f, "password="        & cfg.password
    Print #f, "scheme="          & cfg.scheme
    Print #f, "log_enabled="     & cfg.log_enabled
    Print #f, "log_pm="          & cfg.log_pm
    Print #f, "auto_reconnect="  & cfg.auto_reconnect
    Print #f, "show_timestamps=" & cfg.show_timestamps
    Close #f
End Sub

' -----------------------------------------------------------------------------
' Pane: rebuild sorted snapshot of user_list into pane_items(), clamp sel
' Called lazily when pane_dirty = 1
' -----------------------------------------------------------------------------
Sub pane_refresh()
    pane_dirty = 0
    Dim uc As Long = wins(active_win).user_count
    If uc = 0 Then
        ReDim pane_items(0)
        pane_items(0) = ""
        pane_lb_st.sel      = 0
        pane_lb_st.top_item = 0
        Return
    End If
    ReDim pane_items(uc - 1)
    Dim i As Long
    For i = 0 To uc - 1
        pane_items(i) = wins(active_win).user_list(i)
    Next i
    vt_sort(pane_items(), VT_ASCENDING)
    If pane_lb_st.sel >= uc Then pane_lb_st.sel = uc - 1
    If pane_lb_st.sel < 0   Then pane_lb_st.sel = 0
End Sub

' -----------------------------------------------------------------------------
' Draw
' -----------------------------------------------------------------------------
Sub draw_titlebar()
    Dim lbl_left  As String = " [ VTIRC " & VERSION & " ]"
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

Function mirc_visual_len(s As String) As Long
    Dim i    As Long = 1
    Dim slen As Long = Len(s)
    Dim vlen As Long = 0
    Dim ch   As UByte
    Dim d    As Long
    Do While i <= slen
        ch = Asc(s, i)
        Select Case ch
        Case 3
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
        Case 2, 15, 22, 31
            i += 1
        Case Else
            vlen += 1
            i    += 1
        End Select
    Loop
    Return vlen
End Function

Function mirc_wordwrap(txt As String, wid As Long) As String
    Dim result   As String
    Dim cur_line As String
    Dim cur_vis  As Long = 0
    Dim wrd      As String
    Dim wrd_vis  As Long = 0
    Dim i        As Long = 1
    Dim slen     As Long = Len(txt)
    Dim ch       As UByte
    Dim d        As Long

    Do While i <= slen
        ch = Asc(txt, i)
        Select Case ch
        Case 3
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
        Case 2, 15, 22, 31
            wrd &= Chr(ch) : i += 1
        Case 32
            If cur_vis > 0 AndAlso cur_vis + wrd_vis > wid Then
                If Len(result) > 0 Then result &= Chr(10)
                result   &= cur_line
                cur_line  = wrd
                cur_vis   = wrd_vis
            Else
                cur_line &= wrd
                cur_vis  += wrd_vis
            End If
            wrd = "" : wrd_vis = 0
            If cur_vis < wid Then
                cur_line &= " " : cur_vis += 1
            End If
            i += 1
        Case Else
            wrd     &= Chr(ch)
            wrd_vis += 1
            i       += 1
        End Select
    Loop
    If Len(wrd) > 0 Then
        If cur_vis > 0 AndAlso cur_vis + wrd_vis > wid Then
            If Len(result) > 0 Then result &= Chr(10)
            result   &= cur_line
            cur_line  = wrd
        Else
            cur_line &= wrd
        End If
    End If
    If Len(cur_line) > 0 Then
        If Len(result) > 0 Then result &= Chr(10)
        result &= cur_line
    End If
    Return result
End Function

' Render one history line.  Starts at CHAT_COL, bounded by max_vis.
Sub draw_mirc_line(ln_row As Long, base_fg As UByte, base_bg As UByte, _
                   txt As String, max_vis As Long)
    Dim cur_fg  As UByte = base_fg
    Dim cur_bg  As UByte = base_bg
    Dim rev_vid As Byte  = 0
    Dim prt_buf As String
    Dim i       As Long  = 1
    Dim slen    As Long  = Len(txt)
    Dim vis     As Long  = 0
    Dim ch      As UByte
    Dim d       As Long
    Dim nfg     As Long
    Dim nbg     As Long
    Dim dstr    As String

    vt_locate(ln_row, CHAT_COL)
    vt_color(cur_fg, cur_bg)

    Do While i <= slen AndAlso vis < max_vis
        ch = Asc(txt, i)
        Select Case ch
        Case 3
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
            If nfg >= 0 Then
                cur_fg = mirc_to_vt(nfg)
            Else
                cur_fg = base_fg : cur_bg = base_bg
            End If
            If nbg >= 0 Then cur_bg = mirc_to_vt(nbg)
            If rev_vid Then vt_color(cur_bg, cur_fg) Else vt_color(cur_fg, cur_bg)
        Case 15
            If Len(prt_buf) > 0 Then vt_print(prt_buf) : prt_buf = ""
            cur_fg  = base_fg : cur_bg = base_bg : rev_vid = 0
            vt_color(cur_fg, cur_bg)
            i += 1
        Case 22
            If Len(prt_buf) > 0 Then vt_print(prt_buf) : prt_buf = ""
            rev_vid Xor= 1
            If rev_vid Then vt_color(cur_bg, cur_fg) Else vt_color(cur_fg, cur_bg)
            i += 1
        Case 2, 31
            i += 1
        Case Else
            prt_buf &= Chr(ch)
            vis     += 1
            i       += 1
        End Select
    Loop
    If Len(prt_buf) > 0 Then vt_print(prt_buf)
End Sub

' -----------------------------------------------------------------------------
' url_find_in_plain -- scan a colour-stripped line for the first http(s):// URL.
' Returns 1 on success; u_start and u_end are 1-based positions in plain_txt.
' -----------------------------------------------------------------------------
Function url_find_in_plain(plain_txt As String, ByRef u_start As Long, ByRef u_end As Long) As Byte
    Dim http_pos  As Long = InStr(plain_txt, "http://")
    Dim https_pos As Long = InStr(plain_txt, "https://")

    If http_pos = 0 AndAlso https_pos = 0 Then Return 0

    Dim srch As Long
    If http_pos = 0 Then
        srch = https_pos
    ElseIf https_pos = 0 Then
        srch = http_pos
    Else
        srch = IIf(http_pos < https_pos, http_pos, https_pos)
    End If

    u_start = srch
    Dim ii   As Long = srch
    Dim slen As Long = Len(plain_txt)
    Do While ii <= slen
        Dim uch As UByte = Asc(plain_txt, ii)
        If uch = 32 OrElse uch = 9 Then Exit Do   ' space or tab ends URL
        ii += 1
    Loop
    u_end = ii - 1

    If u_end < u_start Then Return 0
    Return 1
End Function

' -----------------------------------------------------------------------------
' open_url -- launch the system browser for url_txt.
' -----------------------------------------------------------------------------
Sub open_url(url_txt As String)
    If Len(url_txt) = 0 Then Exit Sub
    #ifdef __FB_WIN32__
        Shell "start """" """ & url_txt & """"
    #else
        Shell "xdg-open """ & url_txt & """"
    #endif
End Sub


Sub draw_history()
    Dim row      As Long
    Dim i        As Long
    Dim ri       As Long
    Dim txt_disp As String
    ' fill only the chat columns (leave pane columns untouched)
    vt_tui_rect_fill(CHAT_COL, CHAT_TOP_ROW, CHAT_WIDE, CHAT_ROWS, 32, VT_LIGHT_GREY, col_bg_main)
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

        draw_mirc_line(row, wins(active_win).history(ri).col_fg, col_bg_main, txt_disp, CHAT_WIDE)

        ' --- URL hit-map update -----------------------------------------------
        Dim hm_idx As Long = row - CHAT_TOP_ROW
        url_hit_map(hm_idx).url_str   = ""
        url_hit_map(hm_idx).col_start = 0
        url_hit_map(hm_idx).col_end   = 0

        Dim plain_ln As String = irc_strip_colors(txt_disp)
        Dim pu_s     As Long
        Dim pu_e     As Long
        If url_find_in_plain(plain_ln, pu_s, pu_e) Then
            Dim sc_s As Long = CHAT_COL + pu_s - 1
            Dim sc_e As Long = CHAT_COL + pu_e - 1
            If sc_e > CHAT_COL + CHAT_WIDE - 1 Then sc_e = CHAT_COL + CHAT_WIDE - 1
            If sc_s <= sc_e Then
                url_hit_map(hm_idx).col_start = sc_s
                url_hit_map(hm_idx).col_end   = sc_e
                url_hit_map(hm_idx).url_str   = Mid(plain_ln, pu_s, pu_e - pu_s + 1)
                vt_color(col_fg_link, col_bg_main)
                vt_locate(row, sc_s)
                vt_print(Mid(plain_ln, pu_s, pu_e - pu_s + 1))
            End If
        End If
        ' ----------------------------------------------------------------------

        row += 1
    Next i
End Sub

Sub draw_status()
    Dim hint     As String
    Dim uw_i     As Long
    Dim pm_alert As String = ""
    For uw_i = 0 To win_count - 1
        If uw_i <> active_win AndAlso wins(uw_i).unread Then
            If Len(pm_alert) = 0 Then pm_alert = " [Unread:"
            pm_alert &= " " & wins(uw_i).target
        End If
    Next uw_i
    If Len(pm_alert) > 0 Then
        pm_alert &= "] Ctrl+Tab"
        hint      = pm_alert
    End If
    If is_afk Then
        hint &= " [AFK: " & Left(afk_msg, 10) & "]"
    End If
    If cfg.log_enabled Then
        hint &= " [LOG]"
    End If
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
    Dim prefix  As String = cfg.nick & "> "
    Dim pfx_len As Long   = Len(prefix)
    ' horizontal separator between history and input line
    vt_tui_hline(PANE_SEP+1, ROW_INPUT-1, CHAT_WIDE, col_fg_sys, col_bg_main)

    vt_color(col_fg_body, col_bg_main)
    vt_locate(ROW_INPUT, CHAT_COL)
    vt_print(prefix)
    input_form(0).x   = CHAT_COL + pfx_len
    input_form(0).y   = ROW_INPUT
    input_form(0).wid = SCREEN_COLS - CHAT_COL + 1 - pfx_len
    vt_tui_form_draw(input_form(), input_focused)
End Sub

' Draw the left-side user-list pane: listbox + separator + PM button
Sub draw_pane()
    If pane_dirty Then pane_refresh()

    ' background: rows 2..ROW_INPUT-1 (listbox area + gap row 38)
    vt_tui_rect_fill(1, CHAT_TOP_ROW, PANE_W, ROW_INPUT - CHAT_TOP_ROW, _
                     32, col_fg_body, col_bg_main)

    ' listbox (rows 2..37, height = CHAT_ROWS)
    If wins(active_win).user_count > 0 Then
        vt_tui_listbox_draw(1, CHAT_TOP_ROW, PANE_W, ROW_STATUS - 1, pane_items(), pane_lb_st)
    Else
        vt_color(col_fg_sys, col_bg_main)
        vt_locate(CHAT_TOP_ROW, 1)
        vt_print(vt_str_pad_right(" (no users)", PANE_W, " "))
    End If

    ' vertical separator from title bar row down to ROW_INPUT
    vt_tui_vline(PANE_SEP, 2, ROW_INPUT, col_fg_sys, col_bg_main)
End Sub

Sub draw_all()
    vt_color(col_fg_body, col_bg_main)
    vt_cls(col_bg_main)
    draw_titlebar()
    draw_history()
    draw_pane()    ' drawn after history; separator lives in its own column
    draw_status()
    draw_input()   ' last -- owns the cursor via vt_tui_form_draw
End Sub

' Help window
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
    help_txt &= "  F3          Client settings"          & VT_LF
    help_txt &= "  F4          Browse channels"          & VT_LF
    help_txt &= "  F10         Disconnect"               & VT_LF
    help_txt &= "  Alt+F4      Quit"                     & VT_LF
    help_txt &= "  Ctrl+Tab    Cycle windows"            & VT_LF
    help_txt &= "  Ctrl+W      Close PM window"          & VT_LF
    help_txt &= "   Note: use /part for channels"        & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Scrolling"                             & VT_LF
    help_txt &= "  PgUp/PgDn/Wheel  Scroll history"      & VT_LF
    help_txt &= "  Home             Jump to oldest"      & VT_LF
    help_txt &= "  End              Jump to newest"      & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " User List (left pane)"                 & VT_LF
    help_txt &= "  Shows users in the active channel"    & VT_LF
    help_txt &= "  (Empty when active window is a PM)"   & VT_LF
    help_txt &= "  Click nick to select"                 & VT_LF
    help_txt &= "  Double-click to open PM"              & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Copy from history"                     & VT_LF
    help_txt &= "  1)hold LMB and drag to select"        & VT_LF
    help_txt &= "  2)RMB to copy selection to clipboard" & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Paste from clipboard"                  & VT_LF
    help_txt &= "  MMB in chat line or SHIFT+INS"        & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Commands"                              & VT_LF
    help_txt &= "  /nick <n>       Change nickname"      & VT_LF
    help_txt &= "  /names          List channel users"   & VT_LF
    help_txt &= "  /msg <n> [txt]  Open PM window"       & VT_LF
    help_txt &= "  /me <text>      Send action message"  & VT_LF
    help_txt &= "  /afk [msg]      Set away status"      & VT_LF
    help_txt &= "  /back           Return from AFK"      & VT_LF
    help_txt &= "  /part           Leave active channel" & VT_LF
    help_txt &= "  /join <ch>      Join channel(new win)"& VT_LF
    help_txt &= "  /quit           Disconnect"           & VT_LF
    help_txt &= ""                                       & VT_LF
    help_txt &= " Input History"                         & VT_LF
    help_txt &= "  Up/Down arrows  Browse sent lines"    & VT_LF
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
    vt_tui_theme_default()

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
        vt_sleep(IDLE_MS)
    Loop
    scheme_apply()
End Sub

' -----------------------------------------------------------------------------
' F2 -- Server settings form
' -----------------------------------------------------------------------------
Function settings_server_form() As Long
    Const ITEM_COUNT = 12
    Dim items(0 To ITEM_COUNT - 1) As vt_tui_form_item
    Dim focused As Long = 1
    Dim k       As ULong
    Dim result  As Long

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
    items(4).x = 1 : items(4).y = 5 : items(4).val = "Channels:"
    items(6).x = 1 : items(6).y = 7 : items(6).val = "Nick:"
    items(8).x = 1 : items(8).y = 9 : items(8).val = "Password:"

    items(1).kind    = VT_FORM_INPUT
    items(1).x       = 12 : items(1).y       = 1
    items(1).wid     = 30 : items(1).max_len = 63
    items(1).val     = cfg.server
    items(1).cpos    = Len(cfg.server)

    items(3).kind    = VT_FORM_INPUT
    items(3).x       = 12 : items(3).y       = 3
    items(3).wid     = 8  : items(3).max_len = 5
    items(3).val     = Trim(Str(cfg.port))
    items(3).cpos    = Len(items(3).val)

    items(5).kind    = VT_FORM_INPUT
    items(5).x       = 12 : items(5).y       = 5
    items(5).wid     = 30 : items(5).max_len = 200
    items(5).val     = cfg.channel
    items(5).cpos    = Len(cfg.channel)

    items(7).kind    = VT_FORM_INPUT
    items(7).x       = 12 : items(7).y       = 7
    items(7).wid     = 20 : items(7).max_len = 30
    items(7).val     = cfg.nick
    items(7).cpos    = Len(cfg.nick)

    items(9).kind    = VT_FORM_INPUT
    items(9).x       = 12 : items(9).y       = 9
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
    vt_tui_theme_default()
    Do
        k      = vt_inkey()
        result = vt_tui_form_handle(items(), focused, k)
        vt_tui_rect_fill(FORM_X + 1, FORM_Y + 1, FORM_W - 2, FORM_H - 2, _
                         32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(FORM_X, FORM_Y, FORM_W, FORM_H, " VTIRC Server Settings ", _
                      VT_TUI_WIN_SHADOW)
        vt_tui_form_draw(items(), focused)
        vt_sleep(IDLE_MS)

        Select Case result
        Case VT_FORM_CANCEL, 2
            scheme_apply()
            Return 0
        Case 1
            cfg.server   = Trim(items(1).val)
            cfg.port     = Val(Trim(items(3).val))
            If cfg.port < 1 Or cfg.port > 65535 Then cfg.port = 6667
            cfg.channel  = Trim(items(5).val)
            cfg.nick     = Trim(items(7).val)
            cfg.password = items(9).val
            cfg_save()
            scheme_apply()
            Return 1
        End Select
    Loop
    scheme_apply()
    Return 0
End Function

' -----------------------------------------------------------------------------
' F3 -- Common settings form
' -----------------------------------------------------------------------------
Function settings_common_form() As Long
    Const ITEM_COUNT = 14
    Dim items(0 To ITEM_COUNT - 1) As vt_tui_form_item
    Dim focused As Long = 1
    Dim k       As ULong
    Dim result  As Long

    items(0).kind   = VT_FORM_LABEL
    items(0).x      = 1 : items(0).y   = 1
    items(0).wid    = CFORM_W - 2
    items(0).val    = "Colour Scheme:"
    items(0).align  = VT_ALIGN_LEFT
    items(0).lbl_fg = VT_BLACK
    items(0).lbl_bg = VT_LIGHT_GREY

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

    items(10).kind   = VT_FORM_LABEL
    items(10).x      = 1 : items(10).y   = 11
    items(10).wid    = 10
    items(10).val    = "Display:"
    items(10).align  = VT_ALIGN_RIGHT
    items(10).lbl_fg = VT_BLACK
    items(10).lbl_bg = VT_LIGHT_GREY

    items(11).kind    = VT_FORM_CHECKBOX
    items(11).x       = 12 : items(11).y = 11
    items(11).val     = "Show timestamps [HH:MM]"
    items(11).checked = cfg.show_timestamps

    items(12).kind = VT_FORM_BUTTON
    items(12).x    = 10 : items(12).y = 13
    items(12).val  = "OK"
    items(12).ret  = 1

    items(13).kind = VT_FORM_BUTTON
    items(13).x    = 22 : items(13).y = 13
    items(13).val  = "Cancel"
    items(13).ret  = 2

    vt_tui_form_offset(items(), CFORM_X, CFORM_Y)
    draw_all()
    vt_tui_theme_default()

    Do
        irc_poll()
        k      = vt_inkey()
        result = vt_tui_form_handle(items(), focused, k)
        vt_tui_rect_fill(CFORM_X + 1, CFORM_Y + 1, CFORM_W - 2, CFORM_H - 2, _
                         32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(CFORM_X, CFORM_Y, CFORM_W, CFORM_H, " VTIRC Settings ", _
                      VT_TUI_WIN_SHADOW)
        vt_tui_form_draw(items(), focused)
        vt_sleep(IDLE_MS)

        Select Case result
        Case VT_FORM_CANCEL, 2
            scheme_apply()
            Return 0
        Case 1
            If items(1).checked Then cfg.scheme = 0
            If items(2).checked Then cfg.scheme = 1
            If items(3).checked Then cfg.scheme = 2
            Dim new_log As Byte = items(5).checked
            If cfg.log_enabled = 1 AndAlso new_log = 0 Then
                hist_append("*** Logging stopped.", col_fg_sys)
                cfg.log_enabled = 0
            ElseIf cfg.log_enabled = 0 AndAlso new_log = 1 Then
                cfg.log_enabled = 1
                hist_append("*** Logging started.", col_fg_sys)
            End If
            cfg.log_pm = items(7).checked
            Dim new_ar As Byte = items(9).checked
            If cfg.auto_reconnect = 1 AndAlso new_ar = 0 Then
                reconnect_pending = 0
            End If
            cfg.auto_reconnect  = new_ar
            cfg.show_timestamps = items(11).checked
            cfg_save()
            scheme_apply()
            Return 1
        End Select
    Loop
    scheme_apply()
    Return 0
End Function

' -----------------------------------------------------------------------------
' IRC: send
' -----------------------------------------------------------------------------
Sub irc_send(irc_msg As String)
    If sock_valid = 0 Then Return
    If Len(irc_msg) > 510 Then irc_msg = Left(irc_msg, 510)
    Dim full_msg As String = irc_msg & Chr(13) & Chr(10)
    Dim snd_len  As Long   = Len(full_msg)
    Dim sent     As Long   = 0
    Dim nb       As Long
    Dim zbuf     As ZString * 513
    zbuf = full_msg
    Do While sent < snd_len
        nb = vt_net_send(sock, @zbuf + sent, snd_len - sent)
        If nb <= 0 Then Exit Do
        sent += nb
    Loop
End Sub

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
        Case 3
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
        Case 2, 15, 22, 31
            i += 1
        Case Else
            result &= Chr(ch)
            i += 1
        End Select
    Loop
    Return result
End Function

' -----------------------------------------------------------------------------
' Color input: ^n -> mIRC Chr(3)nn
' -----------------------------------------------------------------------------
Function q3_to_mirc(txt As String) As String
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
    If has_color Then result &= Chr(15)
    Return result
End Function

' -----------------------------------------------------------------------------
' User list  (sorted snapshot rebuilt via pane_refresh on pane_dirty)
' -----------------------------------------------------------------------------
Sub user_list_clear(win_idx As Long)
    wins(win_idx).user_count = 0
    If win_idx = active_win Then pane_dirty = 1
End Sub

Sub user_list_add(win_idx As Long, nick As String)
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
    Dim i As Long
    For i = 0 To wins(win_idx).user_count - 1
        If LCase(wins(win_idx).user_list(i)) = LCase(n) Then Return
    Next i
    If wins(win_idx).user_count >= USER_MAX Then Return
    wins(win_idx).user_list(wins(win_idx).user_count) = n
    wins(win_idx).user_count += 1
    If win_idx = active_win Then pane_dirty = 1
End Sub

Sub user_list_remove(win_idx As Long, nick As String)
    Dim i As Long
    For i = 0 To wins(win_idx).user_count - 1
        If LCase(wins(win_idx).user_list(i)) = LCase(nick) Then
            Dim j As Long
            For j = i To wins(win_idx).user_count - 2
                wins(win_idx).user_list(j) = wins(win_idx).user_list(j + 1)
            Next j
            wins(win_idx).user_list(wins(win_idx).user_count - 1) = ""
            wins(win_idx).user_count -= 1
            If win_idx = active_win Then pane_dirty = 1
            Return
        End If
    Next i
End Sub

Sub user_list_rename(win_idx As Long, old_nick As String, new_nick As String)
    Dim i As Long
    For i = 0 To wins(win_idx).user_count - 1
        If LCase(wins(win_idx).user_list(i)) = LCase(old_nick) Then
            wins(win_idx).user_list(i) = new_nick
            If win_idx = active_win Then pane_dirty = 1
            Return
        End If
    Next i
End Sub

Sub user_list_echo(win_idx As Long)
    If wins(win_idx).user_count = 0 Then
        win_hist_append(win_idx, "*** No users tracked for " & wins(win_idx).target, col_fg_sys)
        Return
    End If
    win_hist_append(win_idx, "*** " & wins(win_idx).user_count & " users in " & wins(win_idx).target & ":", col_fg_sys)
    Dim ln  As String
    Dim i   As Long
    For i = 0 To wins(win_idx).user_count - 1
        Dim entry As String = wins(win_idx).user_list(i)
        If Len(ln) + 1 + Len(entry) > SCREEN_COLS - 4 Then
            win_hist_append(win_idx, "  " & ln, col_fg_sys)
            ln = entry
        Else
            If Len(ln) > 0 Then ln &= " "
            ln &= entry
        End If
    Next i
    If Len(ln) > 0 Then win_hist_append(win_idx, "  " & ln, col_fg_sys)
End Sub

Function user_in_win(win_idx As Long, nick As String) As Byte
    Dim i As Long
    For i = 0 To wins(win_idx).user_count - 1
        If LCase(wins(win_idx).user_list(i)) = LCase(nick) Then Return 1
    Next i
    Return 0
End Function

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
    If pm = 0 Then
        ' Channel window: insert before the first PM window
        Dim insert_at As Long = win_count
        Dim si        As Long
        For si = 0 To win_count - 1
            If wins(si).is_pm Then
                insert_at = si
                Exit For
            End If
        Next si
        ' Shift existing windows right to make room
        For si = win_count - 1 To insert_at Step -1
            wins(si + 1) = wins(si)
        Next si
        ' Adjust active_win if it sits at or after the insertion point
        If active_win >= insert_at Then active_win += 1
        idx = insert_at
    Else
        ' PM window: append at end
        idx = win_count
    End If
    wins(idx).target          = tgt
    wins(idx).is_pm           = pm
    wins(idx).unread          = 0
    wins(idx).hist_count      = 0
    wins(idx).hist_head       = 0
    wins(idx).top_line        = 0
    wins(idx).new_msgs        = 0
    wins(idx).user_count      = 0
    wins(idx).names_receiving = 0
    win_count += 1
    Return idx
End Function

Sub win_close(win_idx As Long)
    If win_count <= 1 Then
        win_hist_append(0, "*** Cannot close the last window. Use /part to leave a channel.", VT_BRIGHT_RED)
        Exit Sub
    End If
    If win_idx < 0 OrElse win_idx >= win_count Then Exit Sub
    Dim i As Long
    For i = win_idx To win_count - 2
        wins(i) = wins(i + 1)
    Next i
    ' Clear the now-vacated slot
    wins(win_count - 1).target          = ""
    wins(win_count - 1).is_pm           = 0
    wins(win_count - 1).unread          = 0
    wins(win_count - 1).hist_count      = 0
    wins(win_count - 1).hist_head       = 0
    wins(win_count - 1).top_line        = 0
    wins(win_count - 1).new_msgs        = 0
    wins(win_count - 1).user_count      = 0
    wins(win_count - 1).names_receiving = 0
    win_count -= 1
    ' Adjust active_win
    If active_win > win_idx Then active_win -= 1
    If active_win >= win_count Then active_win = win_count - 1
    If active_win < 0 Then active_win = 0
    pane_dirty = 1
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

    ' PRIVMSG handler with CTCP ACTION detection
    Case "PRIVMSG"
        Dim priv_tgt  As String = Trim(prms_str)
        Dim priv_wi   As Long   = win_find(priv_tgt)
        ' detect CTCP ACTION  (format: Chr(1) & "ACTION text" & Chr(1))
        Dim is_action  As Byte   = 0
        Dim action_txt As String
        If Left(trail_str, 8) = Chr(1) & "ACTION " Then
            Dim ctcp_end As Long = InStr(2, trail_str, Chr(1))
            If ctcp_end > 0 Then
                action_txt = Mid(trail_str, 9, ctcp_end - 9)
            Else
                action_txt = Mid(trail_str, 9)
            End If
            is_action = 1
        End If
        If priv_wi >= 0 AndAlso wins(priv_wi).is_pm = 0 Then
            Dim msg_fg As UByte
            If LCase(src_nick) = LCase(cfg.nick) Then
                msg_fg = col_fg_own
            Else
                msg_fg = nick_color(src_nick)
            End If
            If is_action Then
                win_hist_append(priv_wi, "* " & src_nick & " " & action_txt, msg_fg)
            Else
                win_hist_append(priv_wi, "<" & src_nick & "> " & trail_str, msg_fg)
            End If
        ElseIf LCase(priv_tgt) = LCase(cfg.nick) Then
            Dim pm_wi As Long = win_open(src_nick, 1)
            If pm_wi >= 0 Then
                If is_action Then
                    win_hist_append(pm_wi, "* " & src_nick & " " & action_txt, nick_color(src_nick))
                Else
                    win_hist_append(pm_wi, "<" & src_nick & "> " & trail_str, nick_color(src_nick))
                End If
            End If
        End If

    Case "NOTICE"
        Dim notice_src  As String = src_nick
        Dim notice_tgt  As String = Trim(prms_str)
        Dim notice_txt  As String = irc_strip_colors(trail_str)
        Dim notice_line As String
        Dim notice_wi   As Long   = win_find(notice_tgt)
        If notice_wi >= 0 AndAlso wins(notice_wi).is_pm = 0 Then
            notice_line = "-" & notice_src & ":" & notice_tgt & "- " & notice_txt
            win_hist_append(notice_wi, notice_line, VT_YELLOW)
        Else
            notice_line = "-" & notice_src & "- " & notice_txt
            hist_append(notice_line, VT_YELLOW)
        End If

    Case "JOIN"
        Dim jch As String = trail_str
        If jch = "" Then jch = prms_str
        jch = Trim(jch)
        Dim join_wi As Long = win_find(jch)
        If join_wi >= 0 Then
            win_hist_append(join_wi, "*** " & src_nick & " has joined " & jch, col_fg_sys)
            user_list_add(join_wi, src_nick)
        Else
            hist_append("*** " & src_nick & " has joined " & jch, col_fg_sys)
        End If

    Case "PART"
        Dim part_ch As String = Trim(prms_str)
        If Len(part_ch) = 0 Then part_ch = Trim(trail_str)
        Dim part_wi As Long = win_find(part_ch)
        If part_wi >= 0 Then
            win_hist_append(part_wi, "*** " & src_nick & " has left " & part_ch, col_fg_sys)
            user_list_remove(part_wi, src_nick)
        Else
            hist_append("*** " & src_nick & " has left " & part_ch, col_fg_sys)
        End If

    Case "QUIT"
        Dim quit_msg As String = irc_strip_colors(trail_str)
        Dim qwi      As Long
        For qwi = 0 To win_count - 1
            If wins(qwi).is_pm = 0 Then
                If user_in_win(qwi, src_nick) Then
                    win_hist_append(qwi, "*** " & src_nick & " has quit (" & quit_msg & ")", col_fg_sys)
                    user_list_remove(qwi, src_nick)
                End If
            Else
                If LCase(wins(qwi).target) = LCase(src_nick) Then
                    win_hist_append(qwi, "*** " & src_nick & " has quit (" & quit_msg & ")", col_fg_sys)
                End If
            End If
        Next qwi

    Case "NICK"
        Dim new_nick As String = irc_strip_colors(trail_str)
        Dim is_self  As Byte   = IIf(LCase(src_nick) = LCase(cfg.nick), 1, 0)
        If is_self Then
            cfg.nick = new_nick
            hist_append("*** You are now known as " & new_nick, col_fg_sys)
        End If
        Dim nwi As Long
        For nwi = 0 To win_count - 1
            If wins(nwi).is_pm = 0 Then
                If user_in_win(nwi, src_nick) Then
                    user_list_rename(nwi, src_nick, new_nick)
                    If is_self = 0 Then
                        win_hist_append(nwi, "*** " & src_nick & " is now known as " & new_nick, col_fg_sys)
                    End If
                End If
            Else
                If LCase(wins(nwi).target) = LCase(src_nick) Then
                    wins(nwi).target = new_nick
                    win_hist_append(nwi, "*** " & src_nick & " is now known as " & new_nick, col_fg_sys)
                End If
            End If
        Next nwi

    Case "001"
        hist_append("*** " & irc_strip_colors(trail_str), col_fg_sys)
        connected = 1
        ' Send JOIN for every channel window that looks like a real channel
        Dim ch001_wi As Long
        For ch001_wi = 0 To win_count - 1
            If wins(ch001_wi).is_pm = 0 Then
                user_list_clear(ch001_wi)
                If Left(wins(ch001_wi).target, 1) = "#" OrElse _
                   Left(wins(ch001_wi).target, 1) = "&" Then
                    irc_send("JOIN " & wins(ch001_wi).target)
                End If
            End If
        Next ch001_wi

    Case "305"
        is_afk  = 0
        afk_msg = ""
        hist_append("*** You are no longer marked as away.", col_fg_sys)
    
    Case "322"
        ' RPL_LIST: prms_str = "yournick #channel usercount"
        If chlist_active AndAlso chlist_count < CHLIST_MAX Then
            Dim tok322()  As String
            Dim tcnt322   As Long = vt_str_split(Trim(prms_str), " ", tok322())
            If tcnt322 >= 3 Then
                Dim ch322  As String = tok322(1)
                Dim cnt322 As Long   = Val(tok322(2))
                If Len(ch322) > 0 Then
                    chlist_names(chlist_count)  = ch322
                    chlist_users(chlist_count)  = cnt322
                    chlist_topics(chlist_count) = irc_strip_colors(trail_str)
                    chlist_count += 1
                End If
            End If
        End If

    Case "323"
        ' RPL_LISTEND
        chlist_done = 1

    Case "332"
        ' Topic reply: prms_str = "yournick #channel"  trail_str = topic text
        Dim ch332       As String
        Dim last_sp332  As Long = 0
        Dim pi332       As Long
        For pi332 = 1 To Len(prms_str)
            If Mid(prms_str, pi332, 1) = " " Then last_sp332 = pi332
        Next pi332
        If last_sp332 > 0 Then
            ch332 = Mid(prms_str, last_sp332 + 1)
        Else
            ch332 = prms_str
        End If
        ch332 = Trim(ch332)
        Dim wi332 As Long = win_find(ch332)
        If wi332 >= 0 Then
            win_hist_append(wi332, "*** Topic: " & irc_strip_colors(trail_str), col_fg_sys)
        Else
            hist_append("*** Topic for " & ch332 & ": " & irc_strip_colors(trail_str), col_fg_sys)
        End If

    Case "306"
        hist_append("*** You are now marked as away.", col_fg_sys)

    Case "353"
        ' Extract channel: last space-delimited token in prms_str
        ' prms_str format is e.g. "mynick = #channel" or "mynick * #channel"
        Dim ch353     As String
        Dim last_sp353 As Long = 0
        Dim pi353     As Long
        For pi353 = 1 To Len(prms_str)
            If Mid(prms_str, pi353, 1) = " " Then last_sp353 = pi353
        Next pi353
        If last_sp353 > 0 Then
            ch353 = Mid(prms_str, last_sp353 + 1)
        Else
            ch353 = prms_str
        End If
        ch353 = Trim(ch353)
        Dim wi353 As Long = win_find(ch353)
        If wi353 >= 0 Then
            If wins(wi353).names_receiving = 0 Then
                user_list_clear(wi353)
                wins(wi353).names_receiving = 1
            End If
            Dim nk_parts353() As String
            Dim nk_count353   As Long = vt_str_split(trail_str, " ", nk_parts353())
            Dim ni353         As Long
            For ni353 = 0 To nk_count353 - 1
                If Len(nk_parts353(ni353)) > 0 Then user_list_add(wi353, nk_parts353(ni353))
            Next ni353
        End If

    Case "366"
        ' Extract channel: last space-delimited token in prms_str
        Dim ch366      As String
        Dim last_sp366 As Long = 0
        Dim pi366      As Long
        For pi366 = 1 To Len(prms_str)
            If Mid(prms_str, pi366, 1) = " " Then last_sp366 = pi366
        Next pi366
        If last_sp366 > 0 Then
            ch366 = Mid(prms_str, last_sp366 + 1)
        Else
            ch366 = prms_str
        End If
        ch366 = Trim(ch366)
        Dim wi366 As Long = win_find(ch366)
        If wi366 >= 0 Then
            wins(wi366).names_receiving = 0
            win_hist_append(wi366, "*** " & wins(wi366).user_count & " users in " & ch366, col_fg_sys)
        Else
            hist_append("*** End of NAMES for " & ch366, col_fg_sys)
        End If
    
    Case "433"
        hist_append("*** Nickname in use: try /nick <newnick>", VT_BRIGHT_RED)

    Case Else
        Dim cmd_num As Long = Val(cmd_up)
        If cmd_num >= 1 AndAlso cmd_num <= 999 Then
            Select Case cmd_num
            Case 372, 375, 376
            Case Else
                If Len(trail_str) > 0 Then
                    hist_append("  " & irc_strip_colors(trail_str), VT_DARK_GREY)
                End If
            End Select
        End If
    End Select
End Sub

' -----------------------------------------------------------------------------
' F4 -- Channel browser
' -----------------------------------------------------------------------------
Sub channel_browser()
    Const BFW  = 78
    Const BFH  = 28
    Const BFX  = (SCREEN_COLS - BFW) \ 2     ' = 11
    Const BFY  = (SCREEN_ROWS - BFH) \ 2     ' = 6
    Const BLBX = BFX + 1                      ' listbox left edge
    Const BLBY = BFY + 3                      ' listbox top (row after header)
    Const BLBW = BFW - 2                      ' = 76
    Const BLBH = 20                           ' listbox visible rows
    Const CH_W  = 22                          ' channel name column width
    Const CNT_W = 6                           ' user count column width
    Const TOP_W = BLBW - CH_W - 1 - CNT_W - 1   ' = 46  topic width

    ' -- request channel list ------------------------------------------------
    chlist_active = 1
    chlist_done   = 0
    chlist_count  = 0
    irc_send("LIST")

    draw_all()
    vt_tui_theme_default()

    ' -- fetch loop: show progress until 323 or timeout ----------------------
    Dim t_start As Double = Timer
    Dim ek      As ULong
    Do
        irc_poll()
        If chlist_done Then Exit Do
        If Timer - t_start > 15.0 Then Exit Do

        vt_tui_rect_fill(BFX + 1, BFY + 1, BFW - 2, BFH - 2, 32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(BFX, BFY, BFW, BFH, " Channel Browser ", VT_TUI_WIN_SHADOW)
        vt_color(VT_BLACK, VT_LIGHT_GREY)
        vt_locate(BFY + 5, BFX + 3)
        vt_print("Fetching channel list...  " & chlist_count & " channels received so far.")
        vt_locate(BFY + 7, BFX + 3)
        vt_print("Press Esc to cancel.")

        ek = vt_inkey()
        If VT_SCAN(ek) = VT_KEY_ESC Then
            chlist_active = 0
            scheme_apply()
            Return
        End If
        vt_sleep(IDLE_MS)
    Loop
    chlist_active = 0

    If chlist_count = 0 Then
        scheme_apply()
        hist_append("*** Channel list empty or request timed out.", col_fg_sys)
        Return
    End If

    ' -- build listbox items -------------------------------------------------
    ReDim lb_items(chlist_count - 1) As String
    Dim bi As Long
    For bi = 0 To chlist_count - 1
        Dim ch_col  As String = chlist_names(bi)
        If Len(ch_col) > CH_W Then ch_col = Left(ch_col, CH_W)
        ch_col = ch_col & Space(CH_W - Len(ch_col))

        Dim cnt_str As String = Trim(Str(chlist_users(bi)))
        Dim cnt_col As String = Space(CNT_W - Len(cnt_str)) & cnt_str   ' right-align

        Dim top_col As String = chlist_topics(bi)
        If Len(top_col) > TOP_W Then top_col = Left(top_col, TOP_W)

        lb_items(bi) = ch_col & " " & cnt_col & " " & top_col
    Next bi

    ' -- two buttons ---------------------------------------------------------
    Dim btn_items(1) As vt_tui_form_item
    Dim btn_focused  As Long = 0

    btn_items(0).kind = VT_FORM_BUTTON
    btn_items(0).x    = (BFW \ 2) - 10
    btn_items(0).y    = BFH - 2
    btn_items(0).val  = "Join"
    btn_items(0).ret  = 1

    btn_items(1).kind = VT_FORM_BUTTON
    btn_items(1).x    = (BFW \ 2) + 4
    btn_items(1).y    = BFH - 2
    btn_items(1).val  = "Cancel"
    btn_items(1).ret  = 2

    vt_tui_form_offset(btn_items(), BFX, BFY)

    Dim lb_st As vt_tui_listbox_state
    lb_st.sel             = 0
    lb_st.top_item        = 0
    lb_st.last_click_item = -1
    lb_st.last_click_time = 0.0
    lb_st.prev_btns       = 0

    Dim k_cb   As ULong
    Dim result As Long

    ' -- browser loop --------------------------------------------------------
    Do
        irc_poll()
        k_cb   = vt_inkey()
        result = vt_tui_form_handle(btn_items(), btn_focused, k_cb, VT_FORM_NO_ESC)
        Dim lb_ret As Long = vt_tui_listbox_handle(BLBX, BLBY, BLBW, BLBH, _
                                                    lb_items(), lb_st, k_cb)
        If lb_ret >= 0 Then result = 1   ' double-click in listbox = join

        If VT_SCAN(k_cb) = VT_KEY_ESC OrElse result = 2 Then Exit Do

        If result = 1 Then
            Dim sel_ch As String = chlist_names(lb_st.sel)
            If Len(sel_ch) > 0 Then
                Dim jn_wi As Long = win_find(sel_ch)
                If jn_wi < 0 Then
                    jn_wi = win_open(sel_ch, 0)
                    If jn_wi >= 0 Then
                        log_load_tail(jn_wi, LOG_TAIL_N)
                        If InStr(LCase(cfg.channel), LCase(sel_ch)) = 0 Then
                            If Len(cfg.channel) > 0 Then cfg.channel &= ","
                            cfg.channel &= sel_ch
                            cfg_save()
                        End If
                    End If
                End If
                If jn_wi >= 0 Then
                    active_win              = jn_wi
                    wins(active_win).unread = 0
                    pane_dirty              = 1
                End If
                irc_send("JOIN " & sel_ch)
            End If
            Exit Do
        End If

        ' -- draw ------------------------------------------------------------
        vt_tui_rect_fill(BFX + 1, BFY + 1, BFW - 2, BFH - 2, 32, VT_BLACK, VT_LIGHT_GREY)
        vt_tui_window(BFX, BFY, BFW, BFH, _
                      " Channel Browser  " & chlist_count & " channels  Enter/DblClick=Join ", _
                      VT_TUI_WIN_SHADOW)

        ' column header
        vt_color(VT_DARK_GREY, VT_LIGHT_GREY)
        vt_locate(BFY + 2, BLBX)
        vt_print(Left(vt_str_pad_right("Channel", CH_W, " ") & " " & _
                      vt_str_pad_right("Users", CNT_W, " ") & " " & "Topic", BLBW))

        ' topic of selected item
        Dim sel_idx   As Long  = lb_st.sel
        If sel_idx < 0 Then sel_idx = 0
        If sel_idx >= chlist_count Then sel_idx = chlist_count - 1
        Dim sel_topic As String = chlist_topics(sel_idx)
        If Len(sel_topic) > BFW - 4 Then sel_topic = Left(sel_topic, BFW - 4)
        vt_color(VT_BLACK, VT_LIGHT_GREY)
        vt_locate(BFY + BFH - 4, BFX + 2)
        vt_print(vt_str_pad_right(sel_topic, BFW - 4, " "))

        vt_tui_listbox_draw(BLBX, BLBY, BLBW, BLBH, lb_items(), lb_st)
        vt_tui_form_draw(btn_items(), btn_focused)
        vt_sleep(IDLE_MS)
    Loop

    scheme_apply()
End Sub

' -----------------------------------------------------------------------------
' IRC: disconnect
' -----------------------------------------------------------------------------
Sub irc_disconnect()
    If sock_valid = 0 Then Return
    reconnect_pending = 0
    irc_send("QUIT :VTIRC")
    vt_net_close(sock)
    vt_net_shutdown()
    sock_valid = 0
    connected  = 0
    recv_buf   = ""
    ' Clear all non-win0 windows
    Dim di As Long
    For di = 1 To win_count - 1
        wins(di).target          = ""
        wins(di).is_pm           = 0
        wins(di).unread          = 0
        wins(di).hist_count      = 0
        wins(di).hist_head       = 0
        wins(di).top_line        = 0
        wins(di).new_msgs        = 0
        wins(di).user_count      = 0
        wins(di).names_receiving = 0
    Next di
    wins(0).unread          = 0
    wins(0).user_count      = 0
    wins(0).names_receiving = 0
    win_count  = 1
    active_win = 0
    pane_dirty = 1
    hist_append("*** Disconnected from " & cfg.server, col_fg_sys)
End Sub

' -----------------------------------------------------------------------------
' IRC: poll incoming data
' -----------------------------------------------------------------------------
Sub irc_poll()
    If sock_valid = 0 Then Return
    Dim tmp_buf  As ZString * 4097
    Dim nb       As Long
    Dim crlf_pos As Long
    Do While vt_net_ready(sock, 0, 0) = 1
        nb = vt_net_recv(sock, @tmp_buf, 4096)
        If nb <= 0 Then
            irc_on_drop()
            Return
        End If
        recv_buf &= Left(tmp_buf, nb)
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

Sub irc_on_drop()
    vt_net_close(sock)
    vt_net_shutdown()
    sock_valid = 0
    connected  = 0
    recv_buf   = ""
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
' IRC: connect
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
    vt_net_nonblocking(sock, 1)

    If is_reconnect = 0 Then
        ' Fresh connect: clear all existing windows, then open one per channel
        Dim ci_clr As Long
        For ci_clr = 0 To win_count - 1
            wins(ci_clr).target          = ""
            wins(ci_clr).is_pm           = 0
            wins(ci_clr).unread          = 0
            wins(ci_clr).hist_count      = 0
            wins(ci_clr).hist_head       = 0
            wins(ci_clr).top_line        = 0
            wins(ci_clr).new_msgs        = 0
            wins(ci_clr).user_count      = 0
            wins(ci_clr).names_receiving = 0
        Next ci_clr
        win_count = 0

        Dim ch_arr() As String
        Dim ch_cnt   As Long = vt_str_split(cfg.channel, ",", ch_arr())
        Dim ci       As Long
        For ci = 0 To ch_cnt - 1
            Dim ch_name As String = Trim(ch_arr(ci))
            If Len(ch_name) > 0 Then
                Dim new_wi As Long = win_open(ch_name, 0)
                If new_wi >= 0 Then log_load_tail(new_wi, LOG_TAIL_N)
            End If
        Next ci
        ' Ensure at least one window always exists
        If win_count = 0 Then win_open(cfg.server, 0)
    Else
        ' Reconnect: preserve history, reset per-window transient state
        Dim rwi As Long
        For rwi = 0 To win_count - 1
            wins(rwi).top_line        = 0
            wins(rwi).new_msgs        = 0
            wins(rwi).unread          = 0
            wins(rwi).user_count      = 0
            wins(rwi).names_receiving = 0
        Next rwi
    End If
    is_reconnect = 0
    active_win   = 0
    pane_dirty   = 1

    If Len(cfg.password) > 0 Then irc_send("PASS " & cfg.password)
    irc_send("NICK " & cfg.nick)
    irc_send("USER " & cfg.nick & " 0 * :VTIRC")

    Return 1
End Function

' -----------------------------------------------------------------------------
' Main
' -----------------------------------------------------------------------------

vt_title("VTIRC " & VERSION)
If vt_screen(VT_SCREEN_100_40, VT_WINDOWED) <> 0 Then End 1
vt_scroll_enable(0)
vt_mouse(1)
vt_on_close(@on_close)
vt_copypaste(VT_ENABLED)
vt_view_print( CHAT_TOP_ROW, CHAT_BOT_ROW+1, CHAT_COL, SCREEN_COLS )

cfg_file = ExePath() & "/.vtirc"
cfg_defaults()
cfg_load()
scheme_apply()

input_form(0).kind    = VT_FORM_INPUT
input_form(0).max_len = 510
input_form(0).val     = ""
input_form(0).cpos    = 0
input_focused         = 0

' initialise pane listbox state
pane_lb_st.sel             = 0
pane_lb_st.top_item        = 0
pane_lb_st.last_click_item = -1
pane_lb_st.last_click_time = 0.0
pane_lb_st.prev_btns       = 0

If vt_file_exists(cfg_file) = 0 Then
    If settings_server_form() = 1 Then irc_connect()
Else
    irc_connect()
End If

' -- main loop ----------------------------------------------------------------

Dim k       As ULong
Dim max_scr As Long
Dim mx      As Long
Dim my      As Long
Dim mb      As Long
Dim whl     As Long
Dim prev_mb As Long = 0   ' previous frame buttons for PM click edge-detect

Do
    ' -- auto-reconnect tick --------------------------------------------------
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

    ' -- mouse state ----------------------------------------------------------
    vt_getmouse(@mx, @my, @mb, @whl)

    ' -- pane listbox: mouse-only (k=0 so keyboard goes to chat input) --------
    Dim pane_ret As Long = VT_FORM_PENDING
    If wins(active_win).user_count > 0 Then
        pane_ret = vt_tui_listbox_handle(1, CHAT_TOP_ROW, PANE_W, CHAT_ROWS, _
                                         pane_items(), pane_lb_st, 0)
    End If

    ' -- PM button click (left-button press edge) or listbox double-click -----
    Dim pm_fire As Byte = 0
    
    If pane_ret >= 0 Then pm_fire = 1   ' double-click in listbox

    If pm_fire AndAlso wins(active_win).user_count > 0 Then
        Dim sel_nick As String = pane_items(pane_lb_st.sel)
        If LCase(sel_nick) <> LCase(cfg.nick) Then
            Dim pm_wi As Long = win_open(sel_nick, 1)
            If pm_wi >= 0 Then
                active_win              = pm_wi
                wins(active_win).unread = 0
            Else
                hist_append("*** Too many windows open (max " & WIN_MAX & ")", VT_BRIGHT_RED)
            End If
        End If
    End If

    ' -- URL click detection (left-button press edge, chat area only) ----------
    Dim url_lclick As Byte = (mb And VT_MOUSE_BTN_LEFT) And _
                             Not (prev_mb And VT_MOUSE_BTN_LEFT)
    If url_lclick AndAlso my >= CHAT_TOP_ROW AndAlso my <= CHAT_BOT_ROW _
                  AndAlso mx >= CHAT_COL Then
        Dim uc_idx As Long = my - CHAT_TOP_ROW
        If Len(url_hit_map(uc_idx).url_str) > 0 Then
            If mx >= url_hit_map(uc_idx).col_start AndAlso _
               mx <= url_hit_map(uc_idx).col_end Then
                open_url(url_hit_map(uc_idx).url_str)
            End If
        End If
    End If
    ' --------------------------------------------------------------------------

    prev_mb = mb

    ' -- Ctrl+Tab / Ctrl+W window management ----------------------------------
    If VT_SCAN(k) = VT_KEY_TAB AndAlso VT_CTRL(k) Then
        If win_count > 1 Then
            active_win = (active_win + 1) Mod win_count
            wins(active_win).unread = 0
            pane_dirty = 1
        End If
    ElseIf VT_CHAR(k) = 23 Then
        ' Close active window only if it is a PM; channels are closed via /part
        If active_win >= 0 AndAlso wins(active_win).is_pm Then
            win_close(active_win)
        End If
    Else
        ' intercept Up/Down for command history before passing to form
        Select Case VT_SCAN(k)
        Case VT_KEY_PGUP, VT_KEY_PGDN, VT_KEY_HOME, VT_KEY_END
            ' handled in scroll section below

        Case VT_KEY_UP
            If cmd_hist_count > 0 Then
                If cmd_hist_pos = -1 Then
                    cmd_hist_pos = cmd_hist_count - 1
                ElseIf cmd_hist_pos > 0 Then
                    cmd_hist_pos -= 1
                End If
                Dim up_ridx As Long = (cmd_hist_head + cmd_hist_pos) Mod CMD_HIST_MAX
                input_form(0).val      = cmd_hist_buf(up_ridx)
                input_form(0).cpos     = Len(input_form(0).val)
                input_form(0).view_off = 0
            End If

        Case VT_KEY_DOWN
            If cmd_hist_pos >= 0 Then
                cmd_hist_pos += 1
                If cmd_hist_pos >= cmd_hist_count Then
                    cmd_hist_pos = -1
                    input_form(0).val      = ""
                    input_form(0).cpos     = 0
                    input_form(0).view_off = 0
                Else
                    Dim dn_ridx As Long = (cmd_hist_head + cmd_hist_pos) Mod CMD_HIST_MAX
                    input_form(0).val      = cmd_hist_buf(dn_ridx)
                    input_form(0).cpos     = Len(input_form(0).val)
                    input_form(0).view_off = 0
                End If
            End If

        Case Else
            vt_tui_form_handle(input_form(), input_focused, k, VT_FORM_NO_ESC)
        End Select
    End If

    ' -- mouse wheel scroll (per active window) --------------------------------
    If whl <> 0 AndAlso mx >= CHAT_COL Then
        max_scr = wins(active_win).hist_count - CHAT_ROWS
        If whl > 0 Then
            If max_scr > 0 AndAlso wins(active_win).top_line < max_scr Then
                wins(active_win).top_line += whl
                If wins(active_win).top_line > max_scr Then wins(active_win).top_line = max_scr
            End If
        Else
            wins(active_win).top_line += whl
            If wins(active_win).top_line < 0 Then wins(active_win).top_line = 0
            If wins(active_win).top_line = 0  Then wins(active_win).new_msgs = 0
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
        If connected Then
            channel_browser()
        Else
            hist_append("*** Connect to a server first to browse channels.", VT_BRIGHT_RED)
        End If

    Case VT_KEY_F10
        If sock_valid Then irc_disconnect()

    ' -- send on Enter --------------------------------------------------------
    Case VT_KEY_ENTER
        If Len(input_form(0).val) > 0 Then
            ' save to command history ring before processing
            Dim ch_slot As Long
            If cmd_hist_count < CMD_HIST_MAX Then
                ch_slot = (cmd_hist_head + cmd_hist_count) Mod CMD_HIST_MAX
                cmd_hist_count += 1
            Else
                ch_slot = cmd_hist_head
                cmd_hist_head = (cmd_hist_head + 1) Mod CMD_HIST_MAX
            End If
            cmd_hist_buf(ch_slot) = input_form(0).val
            cmd_hist_pos = -1

            If Left(input_form(0).val, 1) = "/" Then
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

                Case "ME"
                    If connected AndAlso Len(cmd_arg) > 0 Then
                        Dim me_wire As String = q3_to_mirc(cmd_arg)
                        irc_send("PRIVMSG " & wins(active_win).target & " :" & _
                                 Chr(1) & "ACTION " & me_wire & Chr(1))
                        win_hist_append(active_win, "* " & cfg.nick & " " & me_wire, col_fg_own)
                    ElseIf Len(cmd_arg) = 0 Then
                        hist_append("*** Usage: /me <action text>", VT_BRIGHT_RED)
                    Else
                        hist_append("*** Not connected.", VT_BRIGHT_RED)
                    End If

                Case "NICK"
                    If Len(cmd_arg) > 0 Then irc_send("NICK " & cmd_arg)

                Case "JOIN"
                    Dim join_ch As String = Trim(cmd_arg)
                    If Len(join_ch) > 0 AndAlso connected Then
                        Dim join_wi As Long = win_find(join_ch)
                        If join_wi < 0 Then
                            join_wi = win_open(join_ch, 0)
                            If join_wi >= 0 Then
                                log_load_tail(join_wi, LOG_TAIL_N)
                                ' Append to cfg.channel if not already listed
                                If InStr(LCase(cfg.channel), LCase(join_ch)) = 0 Then
                                    If Len(cfg.channel) > 0 Then cfg.channel &= ","
                                    cfg.channel &= join_ch
                                    cfg_save()
                                End If
                            End If
                        End If
                        If join_wi >= 0 Then
                            active_win = join_wi
                            wins(active_win).unread = 0
                            pane_dirty = 1
                        End If
                        irc_send("JOIN " & join_ch)
                    ElseIf Len(join_ch) = 0 Then
                        hist_append("*** Usage: /join <#channel>", VT_BRIGHT_RED)
                    Else
                        hist_append("*** Not connected.", VT_BRIGHT_RED)
                    End If

                Case "PART"
                    If wins(active_win).is_pm = 0 Then
                        Dim part_tgt As String = wins(active_win).target
                        irc_send("PART " & part_tgt)
                        win_hist_append(active_win, "*** You left " & part_tgt, col_fg_sys)
                        ' Remove from cfg.channel list and save
                        Dim part_arr()  As String
                        Dim part_cnt    As Long = vt_str_split(cfg.channel, ",", part_arr())
                        Dim part_new    As String
                        Dim pi          As Long
                        For pi = 0 To part_cnt - 1
                            If LCase(Trim(part_arr(pi))) <> LCase(part_tgt) Then
                                If Len(part_new) > 0 Then part_new &= ","
                                part_new &= Trim(part_arr(pi))
                            End If
                        Next pi
                        cfg.channel = part_new
                        cfg_save()
                        win_close(active_win)
                    Else
                        win_hist_append(active_win, "*** /part: active window is a PM. Use Ctrl+W to close.", VT_BRIGHT_RED)
                    End If

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
                    user_list_echo(active_win)

                Case "MSG"
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
                Dim wire_txt As String = q3_to_mirc(input_form(0).val)
                irc_send("PRIVMSG " & wins(active_win).target & " :" & wire_txt)
                win_hist_append(active_win, "<" & cfg.nick & "> " & wire_txt, col_fg_own)
                wins(active_win).top_line = 0
                wins(active_win).new_msgs = 0
            End If
            input_form(0).val      = ""
            input_form(0).cpos     = 0
            input_form(0).view_off = 0
        End If
    End Select

    If VT_SCAN(k) = VT_KEY_F4 AndAlso VT_ALT(k) Then quit_flag = 1

    ' -- network poll ---------------------------------------------------------
    irc_poll()

    draw_all()
    vt_sleep(IDLE_MS)
Loop Until quit_flag

If sock_valid Then irc_disconnect()
vt_shutdown()
