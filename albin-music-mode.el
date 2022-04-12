;;; albin-music.el --- 网易云音乐

;; Copyright (C) 2014  Albin

;; Author: Albin <albin@zhuyabindemini>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'json)
(require 'assoc)
(require 'url-http)

(defvar 榜单数组 '(
                   ("云音乐新歌榜"."3779629")
                   ("云音乐热歌榜"."3778678")
                   ("网易原创歌曲榜"."2884035")
                   ("云音乐飙升榜"."19723756")
                   ("云音乐电音榜"."10520166")
                   ("UK排行榜周榜"."180106")
                   ("美国Billboard周榜"."60198")
                   ("Beatport全球电子舞曲榜"."3812895")
                   ("iTunes榜"."11641012")
                   ("日本Oricon周榜"."60131")
                   ("韩国Melon排行榜周榜"."3733003")
                   ("韩国Mnet排行榜周榜"."60255")
                   ("Hit_FM_Top榜"."120001")
                   ("台湾Hito排行榜"."112463")
                   ("中国TOP排行榜_港台榜"."112504")
                   ("中国TOP排行榜_内地榜"."64016")
                   ("香港电台中文歌曲龙虎榜"."10169002")
                   ("Channel_V华语榜"."256172")
                   ("Channel_V欧美榜"."257105")
                   ("Channel_V日韩榜"."256189")
                   ("华语金曲榜"."4395559")
                   ("中国嘻哈榜"."1899724")
                   ("华语巴士音乐榜"."3906086")))

(defcustom albin-music-player "mplayer"
  "Player for douban music."
  :type 'string
  :group 'albin-music)

(defvar 当前榜单序号 0)

(defconst 排行榜查询地址 "http://music.163.com/discover/toplist"
  "获取榜单的地址")

(defconst 歌曲详情查询地址 "http://music.163.com/api/song/detail/"
  "获取mp3 json信息的地址")

(defconst albin-music-buffer-name "Albin云音乐"
  "")

(defvar 当前正在播放歌曲的进程 nil "")
(defvar 当前的播放状态 nil "")
(defvar 曲目数 0 "")
(defvar 播放到第几首 0 "")

(defvar 快捷键 nil
  "docstring")
(setq 快捷键
      (let ((map (make-sparse-keymap)))
        (define-key map "c" '切换榜单)
        (define-key map "g" '跳转到第几首)
        (define-key map "n" '下一首)
        (define-key map "p" '上一首)
        (define-key map "q" '杀掉当前播放进程)
        map))

(defvar 播放列表_JSON数据源 nil "")

(defun 访问云主机 (链接 &optional 回调函数)
  (setq url-cookie-untrusted-urls '(".*"))
  (setq url-request-method "GET")
  (setq url-request-extra-headers
        '(
          ("Host" . "music.163.com")
          ("Content-Type" . "application/x-www-form-urlencoded")
          ("Connection" . "keep-alive")
          ("Accept" . "*/*")
          ("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.76.4 (KHTML, like Gecko) Version/7.0.4 Safari/537.76.4")
          ("Referer" . "http://music.163.com/")
          ("Accept-Language" . "en-us")
                                        ;("Accept-Encoding" . "gzip, deflate") ;; gzip暂时不开
          ))
  (if 回调函数
      (url-retrieve 链接 回调函数)
    (url-retrieve-synchronously 链接)))

(defun 表演者 (信息)
  (let (
        (歌手们
         (cdr (assoc 'artists 信息))))
    (insert-string " (")
    (let ((共 (length  歌手们)))
      (dotimes (某 共)
        (insert-string (concat " "
                               (cdr (assoc 'name (elt 歌手们 某)))))))
    (insert-string " )\n")))



(defun 渲染 ()
  (switch-to-buffer albin-music-buffer-name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-string "Albin云音乐\n")
  (insert (propertize
           "====================================================================================================" 'face '(:foreground "Green")))

  (let (channels
        (榜单名 nil))
    (let ((共 (length 榜单数组)))
      (dotimes (某 共)
        (if (zerop (mod 某 3))
            (progn
              (if (not (zerop 某))
                  (insert channels))
              (setq channels "\n  ")))
        (if (equal 某 当前榜单序号)
            (setq 榜单名 (propertize (format "%-32s" (car (elt 榜单数组 某))) 'face '(:foreground "Green")))
          (setq 榜单名 (propertize (format "%-32s" (car (elt 榜单数组 某))) 'face '(:foreground "Grey80"))))
        (setq channels (concat channels (concat (propertize
                                                 (format "%-3d" 某)
                                                 'face '(:foreground "Green"))
                                                榜单名)))))
    (insert channels))

  (insert (propertize
           "\n====================================================================================================\n" 'face '(:foreground "Green")))
  (insert (format "总曲目数:%d 当前播放到:%d\n\n" 曲目数 (+ 播放到第几首 1)))
  (setq 信息 (上一首歌信息))
  (insert-string (format "上一曲目:%s" 
                         (cdr (assoc 'name 信息))))
  (表演者 信息)

  (setq 信息 (elt 播放列表_JSON数据源 播放到第几首))
  (insert-string "当前播放:")
  (insert (propertize (format "%s" (cdr (assoc 'name 信息))) ' face '(:foreground "Green")))
  (表演者 信息)

  (setq 信息 (下一首歌信息))
  (insert-string (format "下一曲目:%s" 
                         (cdr (assoc 'name 信息))))
  (表演者 信息)

  (setq buffer-read-only t))

(defun 切换榜单 (序号)
  (interactive "n序号:")
  (setq 当前榜单序号 序号)
  (根据榜单生成播放列表 (cdr(nth 序号 榜单数组))))



(defun 上一首歌信息 ()
  (let (某)
    (if (< 播放到第几首 1)
        (setq 某 (- 曲目数 1))
      (setq 某 (- 播放到第几首 1)))
    (elt 播放列表_JSON数据源 某)))

(defun 下一首歌信息 ()
  (let (某)
    (if (>= 播放到第几首 (- 曲目数 1))
        (setq 某 0)
      (setq 某 (+ 播放到第几首 1)))
    (elt 播放列表_JSON数据源 某)))


(defun 上一首 ()
  (interactive)
  (if (< 播放到第几首 1)
      (跳转到第几首 (- 曲目数 1))
    (跳转到第几首 (- 播放到第几首 1))))

(defun 下一首 ()
  (interactive)
  (if (>= 播放到第几首 (- 曲目数 1))
      (跳转到第几首 0)
    (跳转到第几首 (+ 播放到第几首 1)))

  (defun 播放或暂停 ()
    (interactive)
    (if (string-match 当前的播放状态 "正在播放")
        (progn
          (setq 当前的播放状态 "暂停")
          (process-send-string 当前正在播放歌曲的进程 "pause\n"))
      (if (string-match 当前的播放状态 "暂停")
          (progn
            (setq 当前的播放状态 "正在播放")
            (process-send-string 当前正在播放歌曲的进程 "pause\n")))))
  )

(defun 杀掉当前播放进程 ()
  (interactive)
  (when (and 当前正在播放歌曲的进程
             (process-live-p 当前正在播放歌曲的进程))
    (delete-process 当前正在播放歌曲的进程)
    (setq 当前正在播放歌曲的进程 nil)))

(defun 当前歌曲播放结束之后 (播放器 某)
  (when (string-match "\\(finished\\|Exiting\\)" 某)
    (下一首)))

(defun 跳转到第几首 (某)
  (interactive "n序号:")
  (杀掉当前播放进程)
  (setq 播放到第几首 某)
  (setq 当前正在播放歌曲的进程
        (start-process "albin-music-proc"
                       nil
                       albin-music-player
                       (cdr (assoc 'mp3Url (elt 播放列表_JSON数据源 某)))))
  (set-process-sentinel
   当前正在播放歌曲的进程
   '当前歌曲播放结束之后)
  (setq 当前的播放状态 "正在播放")
  (渲染)
  )

(defun 从页面中抓取歌曲列表 (页面)
  (setq 列表 "[")

  (while (setq 位置 (string-match "\\(><a href=\"/song\\?id=\\)\\([0-9]\\{5,10\\}\\)"
                                  页面))
    (if (equal 列表 "[")
        (setq 列表 (concat 列表 (match-string 2 页面)))
      (setq 列表 (concat 列表 "," (match-string 2 页面))))
    ;; 截取页面
    (setq 页面 (substring 页面 (+ 位置 6) -1)) ;; -1 表示末端

    ;; 继续查找
    (setq 位置 (string-match "\\(><a href=\"/song\\?id=\\)\\([0-9]\\{5,10\\}\\)"
                             页面)))
  ;; 补全列表格式
  (concat 列表 "]"))

(defun 解析JSON数据 (数据)
  (setq buffer-file-coding-system 'no-conversion)
  (with-current-buffer 数据
    (goto-char (point-min))
    (if (not (search-forward "songs"))
        (message "get channels failed")
      (setq JSON头 (line-beginning-position))
      (setq JSON尾 (line-end-position))
      (setq JSON数据源 (json-read-from-string
                        (decode-coding-string (buffer-substring-no-properties JSON头 JSON尾) 'utf-8)))))
  (setq 播放列表_JSON数据源 (cdr
                             (assoc 'songs
                                    JSON数据源)))
  (setq 曲目数 (length 播放列表_JSON数据源))
  (setq 播放到第几首 0)
  (跳转到第几首 播放到第几首))

(defun 获取所有歌曲的JSON数据 (歌曲列表)
  (解析JSON数据 (访问云主机 
                 (concat 歌曲详情查询地址 "?ids=" 歌曲列表))))

(defun 根据榜单生成播放列表 (榜单)
  (访问云主机
   (concat 排行榜查询地址 "?id=" 榜单)
   #'(lambda (status &rest args)
       (setq buffer-file-coding-system 'no-conversion)
       (setq 页面中的列表 (从页面中抓取歌曲列表 (buffer-string)))
       (kill-buffer)
       (获取所有歌曲的JSON数据 页面中的列表)
       )))

;; atuoload
(defun albin-music ()
  (interactive)
  (cond
   ((buffer-live-p (get-buffer albin-music-buffer-name))
    (switch-to-buffer albin-music-buffer-name))
   (t
    (set-buffer (get-buffer-create albin-music-buffer-name))
    (albin-music-mode)))
                                        ;   (select-window (display-buffer (current-buffer)))
                                        ;    (delete-other-windows)
  (切换榜单 0))



(defun albin-music-mode ()
  "主mode控制这个buffer"
  (kill-all-local-variables)
  (use-local-map 快捷键)
  (setq major-mode 'albin-music-mode)
  (setq mode-name "Albin云音乐")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (run-hooks 'albin-music-mode-hook))

(provide 'albin-music-mode)
;;; albin-music.el ends here
