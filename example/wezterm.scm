;; below code is almost equal under the comment

(define wezterm (require "wezterm"))

(make-table
 (use_ime true)
 (font_size 9.0)
 (enable_tab_bar false)
 (exit_behavior "Close")
 (colors (make-table
          (foreground "#F8F8F2")
          (background  "#272822")
          (cursor_bg "#F8F8F2")
          (cursor_fg "#272822")
          (cursor_border "#3E4451")

         ;; the foreground color of selected text
          (selection_fg "#F8F8F2")
         ;; the background color of selected text
          (selection_bg "#3E3D31")

         ;; The color of the scrollbar "thumb"; the portion that represents the current viewport
          (scrollbar_thumb "#222222")

         ;; The color of the split lines between panes
          (split "#444444")

          (ansi (make-array "#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"))
          (brights (make-array "#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2")))))


;;local wezterm = require 'wezterm'
;;
;; return {
;;     use_ime = true,
;;     font_size = 9.0,
;;     enable_tab_bar = false,
;;     exit_behavior = "Close",
;;
;;     -- https://github.com/oneKelvinSmith/monokai-emacs
;;     -- reference
;;     colors = {
;;         foreground = "#F8F8F2",
;;         background = "#272822",
;;         cursor_bg = "#F8F8F2",
;;         cursor_fg = "#272822",
;;         cursor_border = "#3E4451",
;;
;;         -- the foreground color of selected text
;;         selection_fg = "#F8F8F2",
;;         -- the background color of selected text
;;         selection_bg = "#3E3D31",
;;
;;         -- The color of the scrollbar "thumb"; the portion that represents the current viewport
;;         scrollbar_thumb = "#222222",
;;
;;         -- The color of the split lines between panes
;;         split = "#444444",
;;
;;         ansi = {"#272822", "#F92672", "#A6E22E", "#E6DB74", "#66D9EF", "#FD5FF0", "#A1EFE4", "#F8F8F2"},
;;         brights = {"#272822", "#F92672", "#A6E22E", "#E6DB74", "#66D9EF", "#FD5FF0", "#A1EFE4", "#F8F8F2"},
;;
;;     },
;; }					;

