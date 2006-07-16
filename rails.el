;;; rails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; X-URL:    https://opensvn.csie.org/mvision/emacs/.emacs.d/rails.el
;; $Id$

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Features

;; * Managment WEBrick/Mongrel
;; * Display color log file
;; * Switch beetwin Action/View
;; * TextMate like snippets (snippets.el)
;; * Automatic generate TAGS in RAILS_ROOT directory
;; * Quick access to configuration files
;; * Search in documentation using ri or chm file
;; * Quick start svn-status in RAILS_ROOT

;;; Install

;; Download
;;  * http://cryptocracy.hn.org/~cartel/elisp/snippet.el
;;  * http://www.webweavertech.com/ovidiu/emacs/find-recursive.txt
;; and place into directory where emacs can find it.
;;
;; Add to your .emacs file:
;;
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))
;;
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;;
;; (require 'rails)
;;
;; If you want to use Mongrel instead of WEBrick, add this to you .emacs file:
;; (setq rails-use-mongrel t)
;;

;;; For Windows users only:

;;  If you want to use CHM file for help (by default used ri), add this to your .emacs file:
;;    (setq rails-chm-file "<full_path_to_rails_chm_manual>")
;;  Download and install KeyHH.exe from http://www.keyworks.net/keyhh.htm
;;
;;  Howto using Textmate Backtracer
;;   1. Place info you .emacs file:
;;
;;   (require 'gnuserv)
;;   (setq gnuserv-frame (selected-frame))
;;   (gnuserv-start)

;;   2. Create into you emacs bin directory txmt.js and place into it:
;;
;;   var wsh = WScript.CreateObject("WScript.Shell");
;;   url = WScript.Arguments(0);
;;   if (url) {
;;     var req = /file:\/\/([^&]+).*&line=([0-9]+)/;
;;     var file = req.exec(url);
;;     if (file[1] && file[2]) {
;;       wsh.Run("<path_to_emacs_bin_directory>/gnuclientw.exe +" + file[2] + " " + file[1]);
;;     }
;;   }

;;   3. Create registry key structure:
;;   HKEY_CLASSES_ROOT
;;   -- *txmt*
;;   ---- (Default) = "URL:TXMT Protocol"
;;   ---- URL Protocol = ""
;;   ---- *shell*
;;   ------ *open*
;;   -------- *command*
;;   ---------- (Default) = "cscript /H:WScript /nologo <path_to_emacs_bin_direcory>\txmt.js %1"

;;; BUGS:

;; Do not use automatic snippent expand, be various problem in mmm-mode.
;; Snippets now bind in <TAB>

;; More howtos, see
;;   * http://scott.elitists.net/users/scott/posts/rails-on-emacs
;;   * http://www.emacswiki.org/cgi-bin/wiki/RubyMode

;;; Changelog:

;; HEAD
;;   * rails-switch-view-action: create [action].rhtml if view not exists
;;   * Remove function rails-ri-at-point and rename rails-ri-start to rails-search-doc
;;   * Check rails-chm-file exists before start keyhh
;;   * Fix not match def, if cursor position at begin of line in rails-switch-to-action
;;   * Remove etags support

;; 2006/02/09 (version 0.3)
;;   * Minor fixes in snippets, add extra "-" (Sanford Barr)
;;   * Fix problem at using TAB key
;;   * Display help using CHM manual
;;   * Fix undefined variable html-mode-abbrev-table in older emacs versions
;;   * Add variable rails-use-another-define-key
;;   * Fix void function indent-or-complete
;;   * Add Mongrel support
;;   * Fix compitation warnings
;;   * Display popup menu at point

;; 2006/02/07 (version 0.2)
;;   * Display color logs using ansi-color
;;   * Revert to using snippet.el
;;   * Automatic create TAGS file in RAILS_ROOT
;;   * add variable rails-use-ctags
;;   * fix problem in rails-create-tags (thanks Sanford Barr)
;;   * lazy load TAGS file

;; 2006/02/06 (version 0.1):
;;   * Cleanup code
;;   * Add menu item "SVN status"
;;   * Add menu item "Search documentation"
;;   * If one action associated to multiple views,
;;     display popup menu to a choice view
;;   * More TextMate snippets
;;   * Using ri to search documentation
;;   * Apply patch from Maiha Ishimura

;;; Code:

(eval-when-compile
  (require 'speedbar)
  (require 'ruby-mode))

(require 'ansi-color)
(require 'snippet)
(require 'etags)
(require 'find-recursive)

(defvar rails-version "0.3")
(defvar rails-ruby-command "ruby")
(defvar rails-webrick-buffer-name "*WEBrick*")
(defvar rails-webrick-port "3000")
(defvar rails-webrick-default-env "development")
(defvar rails-webrick-url (concat "http://localhost:" rails-webrick-port))
(defvar rails-templates-list '("rhtml" "rxml" "rjs"))
(defvar rails-chm-file nil "Path CHM file or nil")
(defvar rails-use-another-define-key nil )
(defvar rails-use-mongrel nil "Non nil using Mongrel, else WEBrick")

(defvar rails-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [rails] (cons "RubyOnRails" (make-sparse-keymap "RubyOnRails")))
    (define-key map [rails svn-status]
      '(menu-item "SVN status"
                  (lambda()
                    (interactive)
                    (svn-status (rails-root))
                    :enable (rails-root))))
    (define-key map [rails tag] '("Update TAGS file" . rails-create-tags))
    (define-key map [rails ri] '("Search documentation" . rails-search-doc))
    (define-key map [rails separator] '("--"))
    (define-key map [rails snip] (cons "Snippets" (make-sparse-keymap "Snippets")))
    (define-key map [rails snip render] (cons "render" (make-sparse-keymap "render")))
    (define-key map [rails snip render sk-ra]  '("render (action)\t(ra)" . rails-snip-ra))
    (define-key map [rails snip render sk-ral] '("render (action,layout)\t(ral)" . rails-snip-ral))
    (define-key map [rails snip render sk-rf]  '("render (file)\t(rf)" . rails-snip-rf))
    (define-key map [rails snip render sk-rfu] '("render (file,use_full_path)\t(rfu)" . rails-snip-rfu))
    (define-key map [rails snip render sk-ri]  '("render (inline)\t(ri)" . rails-snip-ri))
    (define-key map [rails snip render sk-ril] '("render (inline,locals)\t(ril)" . rails-snip-ril))
    (define-key map [rails snip render sk-rit] '("render (inline,type)\t(rit)" . rails-snip-rit))
    (define-key map [rails snip render sk-rl]  '("render (layout)\t(rl)" . rails-snip-rl))
    (define-key map [rails snip render sk-rn]  '("render (nothing)\t(rn)" . rails-snip-rn))
    (define-key map [rails snip render sk-rns] '("render (nothing,status)\t(rns)" . rails-snip-rns))
    (define-key map [rails snip render sk-rp]  '("render (partial)\t(rp)" . rails-snip-rp))
    (define-key map [rails snip render sk-rpc] '("render (partial,collection)\t(rpc)" . rails-snip-rpc))
    (define-key map [rails snip render sk-rpl] '("render (partial,locals)\t(rpl)" . rails-snip-rpl))
    (define-key map [rails snip render sk-rpo] '("render (partial,object)\t(rpo)" . rails-snip-rpo))
    (define-key map [rails snip render sk-rps] '("render (partial,status)\t(rps)" . rails-snip-rps))
    (define-key map [rails snip render sk-rt] '("render (text)\t(rt)" . rails-snip-rt))
    (define-key map [rails snip render sk-rtl] '("render (text,layout)\t(rtl)" . rails-snip-rtl))
    (define-key map [rails snip render sk-rtlt] '("render (text,layout => true)\t(rtlt)" . rails-snip-rtlt))
    (define-key map [rails snip render sk-rcea] '("render_component (action)\t(rcea)" . rails-snip-rcea))
    (define-key map [rails snip render sk-rcec] '("render_component (controller)\t(rcec)" . rails-snip-rcec))
    (define-key map [rails snip render sk-rceca] '("render_component (controller, action)\t(rceca)" . rails-snip-rceca))

    (define-key map [rails snip redirect_to] (cons "redirect_to" (make-sparse-keymap "redirect_to")))
    (define-key map [rails snip redirect_to sk-rea] '("redirect_to (action)\t(rea)" . rails-snip-rea))
    (define-key map [rails snip redirect_to sk-reai] '("redirect_to (action, id)\t(reai)" . rails-snip-reai))
    (define-key map [rails snip redirect_to sk-rec] '("redirect_to (controller)\t(rec)" . rails-snip-rec))
    (define-key map [rails snip redirect_to sk-reca] '("redirect_to (controller, action)\t(reca)" . rails-snip-reca))
    (define-key map [rails snip redirect_to sk-recai] '("redirect_to (controller, action, id)\t(recai)" . rails-snip-recai))

    (define-key map [rails snip controller] (cons "controller" (make-sparse-keymap "controller")))
    (define-key map [rails snip controller sk-flash] '("flash[...]\t(flash)" . rails-snip-flash))
    (define-key map [rails snip controller sk-logi] '("logger.info\t(logi)" . rails-snip-logi))
    (define-key map [rails snip controller sk-params] '("params[...]\t(par)" . rails-snip-params))
    (define-key map [rails snip controller sk-session] '("session[...]\t(ses)" . rails-snip-session))

    (define-key map [rails snip model] (cons "model" (make-sparse-keymap "model")))
    (define-key map [rails snip model sk-belongs_to] '("belongs_to (class_name,foreign_key)\t(belongs)" . rails-snip-ar-belongs_to))
    (define-key map [rails snip model sk-has_many] '("has_many (class_name,foreign_key,dependent)\t(many)" . rails-snip-ar-has_many))
    (define-key map [rails snip model sk-has_one] '("has_one (class_name,foreign_key,dependent)\t(one)" . rails-snip-ar-has_one))
    (define-key map [rails snip model sk-val_pres] '("validates_presence_of\t(valpres)" . rails-snip-ar-val_pres))
    (define-key map [rails snip model sk-val_uniq] '("validates_uniqueness_of\t(valuniq)" . rails-snip-ar-val_uniq))
    (define-key map [rails snip model sk-val_num] '("validates_numericality_of\t(valnum)" . rails-snip-ar-val_num))

    (define-key map [rails snip rhtml] (cons "rhtml" (make-sparse-keymap "rhtml")))
    (define-key map [rails snip rhtml sk-erb-ft] '("form_tag\t(ft)" . rails-snip-erb-ft))
    (define-key map [rails snip rhtml sk-erb-lia] '("link_to (action)\t(lia)" . rails-snip-erb-lia))
    (define-key map [rails snip rhtml sk-erb-liai] '("link_to (action, id)\t(liai)" . rails-snip-erb-liai))
    (define-key map [rails snip rhtml sk-erb-lic] '("link_to (controller)\t(lic)" . rails-snip-erb-lic))
    (define-key map [rails snip rhtml sk-erb-lica] '("link_to (controller, action)\t(lica)" . rails-snip-erb-lica))
    (define-key map [rails snip rhtml sk-erb-licai] '("link_to (controller, action, id)\t(licai)" . rails-snip-erb-licai))
    (define-key map [rails snip rhtml sk-erb-ft] '("form_tag\t(ft)" . rails-snip-erb-ft))
    (define-key map [rails snip rhtml sk-erb-h] '("<% h ... %>\t(%h)" . rails-snip-erb-h))
    (define-key map [rails snip rhtml sk-erb-if] '("<% if/end %>\t(%if)" . rails-snip-erb-if))
    (define-key map [rails snip rhtml sk-erb-unless] '("<% unless/end %>\t(%unless)" . rails-snip-erb-unless))
    (define-key map [rails snip rhtml sk-erb-ifel] '("<% if/else/end %>\t(%ifel)" . rails-snip-erb-ifel))
    (define-key map [rails snip rhtml sk-erb-block] '("<% ... %>\t(%)" . rails-snip-erb-block))
    (define-key map [rails snip rhtml sk-erb-echo-block] '("<%= ... %>\t(%%)" . rails-snip-erb-echo-block))

    (define-key map [rails log] (cons "Open log" (make-sparse-keymap "Open log")))
    (define-key map [rails log test]
      '("test.log" . (lambda() (interactive) (rails-open-log "test"))))
    (define-key map [rails log pro]
      '("production.log" . (lambda() (interactive) (rails-open-log "production"))))
    (define-key map [rails log dev]
      '("development.log" . (lambda() (interactive) (rails-open-log "development"))))

    (define-key map [rails config] (cons "Configuration" (make-sparse-keymap "Configuration")))
    (define-key map [rails config routes]
      '("routes.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/routes.rb")))))))
    (define-key map [rails config environment]
      '("environment.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environment.rb")))))))
    (define-key map [rails config database]
      '("database.yml" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/database.yml")))))))
    (define-key map [rails config boot]
      '("boot.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/boot.rb")))))))

    (define-key map [rails config env] (cons "environments" (make-sparse-keymap "environments")))
    (define-key map [rails config env test]
      '("test.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/test.rb")))))))
    (define-key map [rails config env production]
      '("production.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/production.rb")))))))
    (define-key map [rails config env development]
      '("development.rb" .
        (lambda()
          (interactive)
          (let ((root (rails-root)))
            (if root (find-file (concat root "config/environments/development.rb")))))))

    (define-key map [rails webrick] (cons "WEBrick" (make-sparse-keymap "WEBrick")))

    (define-key map [rails webrick mongrel]
      '(menu-item "Use Mongrel" rails-toggle-use-mongrel
                  :enable (not (rails-webrick-process-status))
                  :button (:toggle
                           . (and (boundp 'rails-use-mongrel)
                                   rails-use-mongrel))))

    (define-key map [rails webrick separator] '("--"))

    (define-key map [rails webrick buffer]
      '(menu-item "Show buffer"
                  rails-webrick-open-buffer
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick url]
      '(menu-item "Open browser"
                  rails-webrick-open-browser
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick stop]
      '(menu-item "Stop"
                  rails-webrick-process-stop
                  :enable (rails-webrick-process-status)))
    (define-key map [rails webrick test]
      '(menu-item "Start test"
                  (lambda() (interactive)
                    (rails-webrick-process "test"))
                  :enable (not (rails-webrick-process-status))))
    (define-key map [rails webrick production]
      '(menu-item "Start production"
                  (lambda() (interactive)
                    (rails-webrick-process "production"))
                  :enable (not (rails-webrick-process-status))))
    (define-key map [rails webrick development]
      '(menu-item "Start development"
                  (lambda() (interactive)
                    (rails-webrick-process "development"))
                  :enable (not (rails-webrick-process-status))))

    (define-key map [rails switch-va] '("Switch Action/View" . rails-switch-view-action))

    map))

(defun rails-snip-ra () (interactive)
  (snippet-insert "render :action => \"$${action}\""))

(defun rails-snip-ral () (interactive)
  (snippet-insert "render :action => \"$${action}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rf () (interactive)
  (snippet-insert "render :file => \"$${filepath}\""))

(defun rails-snip-rfu () (interactive)
  (snippet-insert
   "render :file => \"$${filepath}\", :use_full_path => $${false}"))

(defun rails-snip-ri () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\""))

(defun rails-snip-ril () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"))

(defun rails-snip-rit () (interactive)
  (snippet-insert
   "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"))

(defun rails-snip-rl () (interactive)
  (snippet-insert
   "render :layout => \"$${layoutname}\""))

(defun rails-snip-rn () (interactive)
  (snippet-insert
   "render :nothing => $${true}"))

(defun rails-snip-rns () (interactive)
  (snippet-insert
   "render :nothing => $${true}, :status => $${401}" ))

(defun rails-snip-rp () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\""))

(defun rails-snip-rpc () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :collection => $${items}"))

(defun rails-snip-rpl () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"))

(defun rails-snip-rpo () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :object => $${object}"))

(defun rails-snip-rps () (interactive)
  (snippet-insert
   "render :partial => \"$${item}\", :status => $${500}"))

(defun rails-snip-rt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\""))

(defun rails-snip-rtl () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""))

(defun rails-snip-rtlt () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :layout => $${true}"))

(defun rails-snip-rts () (interactive)
  (snippet-insert
   "render :text => \"$${Text here...}\", :status => $${401}"))

(defun rails-snip-rcea () (interactive)
  (snippet-insert
   "render_component :action => \"$${index}\""))

(defun rails-snip-rcec () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\""))

(defun rails-snip-rceca () (interactive)
  (snippet-insert
   "render_component :controller => \"$${items}\", :action => \"$${index}\""))

(defun rails-snip-rea () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${index}\""))

(defun rails-snip-reai () (interactive)
  (snippet-insert
   "redirect_to :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-rec () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\""))

(defun rails-snip-reca () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${list}\""))

(defun rails-snip-recai () (interactive)
  (snippet-insert
   "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"))

(defun rails-snip-flash () (interactive)
  (snippet-insert
   "flash[:$${notice}] = \"$${Text here...}\""))

(defun rails-snip-logi () (interactive)
  (snippet-insert
   "logger.info \"$${Text here...}\""))

(defun rails-snip-params () (interactive)
  (snippet-insert
   "params[:$${id}]"))

(defun rails-snip-session () (interactive)
  (snippet-insert
   "session[:$${user}]"))

(defun rails-snip-ar-belongs_to () (interactive)
  (snippet-insert
   "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""))

(defun rails-snip-ar-has_many () (interactive)
  (snippet-insert
   "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-has_one () (interactive)
  (snippet-insert
   "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"))

(defun rails-snip-ar-val_pres () (interactive)
  (snippet-insert
   "validates_presence_of :$${attr}"))

(defun rails-snip-ar-val_uniq () (interactive)
  (snippet-insert
   "validates_uniqueness_of :$${attr}"))

(defun rails-snip-ar-val_num () (interactive)
  (snippet-insert
   "validates_numericality_of :$${attr}"))

(defun rails-snip-erb-ft () (interactive)
  (snippet-insert
   "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"))

(defun rails-snip-erb-lia () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-liai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-lic () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"))

(defun rails-snip-erb-lica () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"))

(defun rails-snip-erb-licai () (interactive)
  (snippet-insert
   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"))

(defun rails-snip-erb-h () (interactive)
  (snippet-insert "<% h $${@item} %>"))

(defun rails-snip-erb-if () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-ifel () (interactive)
  (snippet-insert "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"))

(defun rails-snip-erb-unless () (interactive)
  (snippet-insert "<% unless $${cond} -%>\n$.\n<% end -%>"))

(defun rails-snip-erb-block () (interactive)
  (snippet-insert "<% $. -%>"))

(defun rails-snip-erb-echo-block () (interactive)
  (snippet-insert "<%= $. %>"))

(defun rails-abbrev-init ()
  "Initialize ruby abbrev table"
  (define-abbrev ruby-mode-abbrev-table "ra"  "" 'rails-snip-ra)
  (define-abbrev ruby-mode-abbrev-table "ral" "" 'rails-snip-ral)
  (define-abbrev ruby-mode-abbrev-table "rf"  "" 'rails-snip-rf)
  (define-abbrev ruby-mode-abbrev-table "rfu" "" 'rails-snip-rfu)
  (define-abbrev ruby-mode-abbrev-table "ri"  "" 'rails-snip-ri)
  (define-abbrev ruby-mode-abbrev-table "ril" "" 'rails-snip-ril)
  (define-abbrev ruby-mode-abbrev-table "rit" "" 'rails-snip-rit)
  (define-abbrev ruby-mode-abbrev-table "rl"  "" 'rails-snip-rl)
  (define-abbrev ruby-mode-abbrev-table "rn"  "" 'rails-snip-rn)
  (define-abbrev ruby-mode-abbrev-table "rns"  "" 'rails-snip-rns)
  (define-abbrev ruby-mode-abbrev-table "rp"  "" 'rails-snip-rp)
  (define-abbrev ruby-mode-abbrev-table "rpc"  "" 'rails-snip-rpc)
  (define-abbrev ruby-mode-abbrev-table "rpl"  "" 'rails-snip-rpl)
  (define-abbrev ruby-mode-abbrev-table "rpo"  "" 'rails-snip-rpo)
  (define-abbrev ruby-mode-abbrev-table "rps"  "" 'rails-snip-rps)
  (define-abbrev ruby-mode-abbrev-table "rt"  "" 'rails-snip-rt)
  (define-abbrev ruby-mode-abbrev-table "rtl"  "" 'rails-snip-rtl)
  (define-abbrev ruby-mode-abbrev-table "rtlt"  "" 'rails-snip-rtlt)
  (define-abbrev ruby-mode-abbrev-table "rts"  "" 'rails-snip-rts)
  (define-abbrev ruby-mode-abbrev-table "rcea"  "" 'rails-snip-rcea)
  (define-abbrev ruby-mode-abbrev-table "rcec"  "" 'rails-snip-rcec)
  (define-abbrev ruby-mode-abbrev-table "rceca"  "" 'rails-snip-rceca)
  (define-abbrev ruby-mode-abbrev-table "rea"  "" 'rails-snip-rea)
  (define-abbrev ruby-mode-abbrev-table "reai"  "" 'rails-snip-reai)
  (define-abbrev ruby-mode-abbrev-table "rec"  "" 'rails-snip-rec)
  (define-abbrev ruby-mode-abbrev-table "reca"  "" 'rails-snip-reca)
  (define-abbrev ruby-mode-abbrev-table "recai"  "" 'rails-snip-recai)
  (define-abbrev ruby-mode-abbrev-table "flash"  "" 'rails-snip-flash)
  (define-abbrev ruby-mode-abbrev-table "logi"  "" 'rails-snip-logi)
  (define-abbrev ruby-mode-abbrev-table "ses"  "" 'rails-snip-session)
  (define-abbrev ruby-mode-abbrev-table "par"  "" 'rails-snip-params)

  (define-abbrev ruby-mode-abbrev-table "belongs"  "" 'rails-snip-ar-belongs_to)
  (define-abbrev ruby-mode-abbrev-table "many"  "" 'rails-snip-ar-has_many)
  (define-abbrev ruby-mode-abbrev-table "one"  "" 'rails-snip-ar-has_one)
  (define-abbrev ruby-mode-abbrev-table "valpres"  "" 'rails-snip-ar-val_pres)
  (define-abbrev ruby-mode-abbrev-table "valuniq"  "" 'rails-snip-ar-val_uniq)
  (define-abbrev ruby-mode-abbrev-table "valnum"  "" 'rails-snip-ar-val_num))


(defun rails-erb-abbrev-init()
  ;; fix undefuned variable html-mode-abbrev-table
  (unless (boundp 'html-mode-abbrev-table)
    (setq html-mode-abbrev-table (make-abbrev-table)))
  (define-abbrev html-mode-abbrev-table "ft"  "" 'rails-snip-erb-ft)
  (define-abbrev html-mode-abbrev-table "lia"  "" 'rails-snip-erb-lia)
  (define-abbrev html-mode-abbrev-table "liai"  "" 'rails-snip-erb-liai)
  (define-abbrev html-mode-abbrev-table "lic"  "" 'rails-snip-erb-lic)
  (define-abbrev html-mode-abbrev-table "lica"  "" 'rails-snip-erb-lica)
  (define-abbrev html-mode-abbrev-table "licai"  "" 'rails-snip-erb-licai)
  (define-abbrev html-mode-abbrev-table "%h"  "" 'rails-snip-erb-h)
  (define-abbrev html-mode-abbrev-table "%if"  "" 'rails-snip-erb-if)
  (define-abbrev html-mode-abbrev-table "%unless"  "" 'rails-snip-erb-unless)
  (define-abbrev html-mode-abbrev-table "%ifel"  "" 'rails-snip-erb-ifel)
  (define-abbrev html-mode-abbrev-table "%"  "" 'rails-snip-erb-block)
  (define-abbrev html-mode-abbrev-table "%%"  "" 'rails-snip-erb-echo-block))

(defun ruby-indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if snippet
      (snippet-next-field)
    (if (looking-at "\\>")
        (hippie-expand nil)
      (ruby-indent-command))))


(defun ruby-newline-and-indent ()
  (interactive)
  (newline)
  (ruby-indent-command))


(defun rails-switch-to-view()
  (let ((pos (nth 2 (posn-at-point)))) ; mouse position at point
    (save-excursion
      (let (action path files)
        (goto-char (line-end-position))
        (search-backward-regexp "^[ ]*def \\([a-z_]+\\)")
        (setq action (match-string 1))
        (search-backward-regexp "^[ ]*class \\([a-zA-Z0-9_]+\\(::\\([a-zA-Z0-9_]+\\)\\)?\\)Controller[ ]+<")
        (setq path (rails-inflector-underscore (match-string 1)))
        (setq path (concat "app/views/" path "/"))

        (setq files (directory-files
                     (concat (rails-root) path)
                     nil
                     (concat "^" action (rails-make-template-regex))))

        (if (= 1 (list-length files))
            (progn
              (find-file (concat (rails-root) path (car files)))
              (message (concat path action))))

        (if (< 1 (list-length files))
            (let (items tmp file)
              (setq tmp files)
              (setq items (list))
              (while (car tmp)
                (add-to-list 'items (cons (car tmp) (car tmp)))
                (setq tmp (cdr tmp)))

              (setq file
                    (x-popup-menu
                     (list (list (car pos) (cdr pos))
                           (selected-window))
                     (list "Please select.." (cons "Please select.." items ))))
              (if file
                  (progn
                    (find-file (concat (rails-root) path file))
                    (message (concat path action))))))

        (if (> 1 (list-length files))
            (if (y-or-n-p (format "%s%s not found, create %s.rhtml? " path action action))
                (let ((root (rails-root)))
                  (make-directory (concat root path) t)
                  (find-file (format "%s%s%s.rhtml" root path action)))))))))


(defun rails-switch-to-action()
  (let (file path action root)
    (setq file buffer-file-name)
    (string-match "views/\\([^/]+\\)/\\([^/\.]+\\)\\(/\\([^/\.]+\\)\\)?" file)
    (if (match-beginning 4)
        (progn
          (setq path
                (concat (substring file (match-beginning 1) (match-end 1))
                        "/"
                        (substring file (match-beginning 2) (match-end 2)) ))
          (setq path (concat path "_controller.rb"))
          (setq action (substring file (match-beginning 4) (match-end 4))))
      (progn
        (setq path (concat
                    (substring file (match-beginning 1) (match-end 1))
                    "_controller.rb" ))
        (setq action (substring file (match-beginning 2) (match-end 2))))
      )
    (setq root (rails-root))
    (setq path (concat "app/controllers/" path))
    (if (file-exists-p (concat root path))
        (progn
          (find-file (concat root path))
          (goto-char (point-min))
          (message (concat path "#" action))
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action))
              (recenter))))))


(defun rails-switch-view-action()
  (interactive)
  (if (string-match "\\.rb$" buffer-file-name)
      (rails-switch-to-view)
    (rails-switch-to-action)))


(defun rails-inflector-underscore (camel-cased-word)
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string "::" "/" camel-cased-word))
         (path (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" path))
         (path (replace-regexp-in-string "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2" path)))
    (downcase path)))


(defun rails-make-template-regex ()
  "Return regex to match rails view templates"
  (let (reg tmp it)
    (setq reg "\\.\\(")
    (setq tmp rails-templates-list)
    (while (setq it (car tmp))
      (progn
        (setq reg (concat reg it))
        (setq tmp (cdr tmp))
        (if (car tmp)
            (setq reg (concat reg "\\|"))
          (setq reg (concat reg "\\)$")))))
    (if reg reg)))


(defun rails-root ()
  "Return RAILS_ROOT"
  (let (curdir max found)
    (setq curdir default-directory)
    (setq max 10)
    (setq found nil)
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "config/environment.rb"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found curdir)))


;; replace in autorevert.el
(defun auto-revert-tail-handler ()
  (let ((size (nth 7 (file-attributes buffer-file-name)))
        (modified (buffer-modified-p))
        buffer-read-only    ; ignore
        (file buffer-file-name)
        buffer-file-name)   ; ignore that file has changed
    (when (> size auto-revert-tail-pos)
      (undo-boundary)
      (save-restriction
        (widen)
        (save-excursion
          (let ((cur-point (point-max)))
            (goto-char (point-max))
            (insert-file-contents file nil auto-revert-tail-pos size)
            (ansi-color-apply-on-region cur-point (point-max)))))
      (undo-boundary)
      (setq auto-revert-tail-pos size)
      (set-buffer-modified-p modified)))
  (set-visited-file-modtime))

(defun rails-open-log (env)
  (let ((root (rails-root)))
    (if root
        (progn
          (if (file-exists-p (concat root "/log/" env ".log"))
              (progn
                (find-file (concat root "/log/" env ".log"))
                (set-buffer-file-coding-system 'utf-8)
                (ansi-color-apply-on-region (point-min) (point-max))
                (set-buffer-modified-p nil)
                (rails-minor-mode t)
                (goto-char (point-max))
                (setq auto-revert-interval 1)
                (setq auto-window-vscroll t)
                (auto-revert-tail-mode t)))))))


(defun rails-webrick-open-browser()
  (interactive)
  (browse-url rails-webrick-url))


(defun rails-webrick-open-buffer()
  (interactive)
  (switch-to-buffer rails-webrick-buffer-name))


(defun rails-webrick-sentinel (proc msg)
  (if (memq (process-status proc) '(exit signal))
        (message
         (concat
          (if rails-use-mongrel "Mongrel" "WEBrick") " stopped"))))


(defun rails-webrick-process-status()
  (let (st)
    (setq st (get-buffer-process rails-webrick-buffer-name))
    (if st t nil)))

(defun rails-webrick-process-stop()
  (interactive)
  (let (proc)
    (setq proc (get-buffer-process rails-webrick-buffer-name))
    (if proc
        (kill-process proc))))


(defun rails-webrick-process(env)
  (let (proc dir root)
    (setq proc (get-buffer-process rails-webrick-buffer-name))
    (unless proc
      (progn
        (setq root (rails-root))
        (if root
            (progn
              (setq dir default-directory)
              (setq default-directory root)
              (if rails-use-mongrel
                  (setq proc
                        (apply 'start-process-shell-command
                               "mongrel_rails"
                               rails-webrick-buffer-name
                               "mongrel_rails"
                               (list "start" "0.0.0.0" rails-webrick-port)))
                (setq proc
                      (apply 'start-process-shell-command
                             rails-ruby-command
                             rails-webrick-buffer-name
                             rails-ruby-command
                             (list (concat root "script/server")
                                   (concat " -e " env)
                                   (concat " -p " rails-webrick-port)))))
              (set-process-filter proc 'rails-webrick-filter)
              (set-process-sentinel proc 'rails-webrick-sentinel)
              (setq default-directory dir)

              (message (concat (if rails-use-mongrel
                                   "Mongrel" "Webrick")
                               "(" env  ") started with port " rails-webrick-port)))
          (progn
            (message "RAILS_ROOT not found")))))))


(defun rails-webrick-filter (process line)
  (let ((buffer (current-buffer)))
    (switch-to-buffer rails-webrick-buffer-name)
    (goto-char(point-min))
    (insert line)
    (switch-to-buffer buffer)))


(defun rails-search-doc (&rest item)
  (interactive)
  (if (or (not (boundp item))
          (not item))
      (setq item (thing-at-point 'sexp)))
  (unless item
    (setq item (read-string "Search symbol? ")))
  (if item
      (let ((buf (buffer-name)))
        (if (and rails-chm-file
                 (file-exists-p rails-chm-file))
            (progn
              (start-process "keyhh" "*keyhh*" "keyhh.exe" "-#klink"
                             (format "'%s'" item)  rails-chm-file))
            (progn
              (unless (string= buf "*ri*")
                (switch-to-buffer-other-window "*ri*"))
              (setq buffer-read-only nil)
              (kill-region (point-min) (point-max))
              (message (concat "Please wait..."))
              (call-process "ri" nil "*ri*" t item)
              (setq buffer-read-only t)
              (local-set-key [return] 'rails-search-doc)
              (goto-char (point-min)))))))


(defun rails-create-tags()
  "Create tags file"
  (interactive)
  (let ((root (rails-root)) dir cmd)
    (message "Creating TAGS, please wait...")
    (setq dir default-directory)
    (setq default-directory root)
    (setq cmd "ctags -e -a --Ruby-kinds=-f -o %s -R %s %s")

    (shell-command (format cmd tags-file-name (concat root "app") (concat root "lib")))

    (setq default-directory dir)
    (visit-tags-table tags-file-name)))


(defun rails-toggle-use-mongrel()
  (interactive)
  (let ((toggle (boundp 'rails-use-mongrel)))
    (setq rails-use-mongrel (not rails-use-mongrel))))


(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  (list
   (cons [menu-bar] rails-minor-mode-menu-bar-map)
   (cons "\C-t"  'rails-switch-view-action)
   (cons [f1]  'rails-search-doc))

  (abbrev-mode -1)
  (rails-abbrev-init)

  ;; Tags
  (make-local-variable 'tags-file-name)

  (setq tags-file-name (concat (rails-root) "TAGS")))

(add-hook 'ruby-mode-hook
          (lambda()
            (rails-minor-mode t)
            (local-set-key (kbd "C-.") 'complete-tag)
            (if rails-use-another-define-key
                (progn
                  (local-set-key (kbd "TAB") 'ruby-indent-or-complete)
                  (local-set-key (kbd "RET") 'ruby-newline-and-indent))
              (progn
                (local-set-key (kbd "<tab>") 'ruby-indent-or-complete)
                (local-set-key (kbd "<return>") 'ruby-newline-and-indent)))))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.rb")))

(add-hook 'find-file-hooks
          (lambda()
            (if (and (string-match (rails-make-template-regex) buffer-file-name)
                     (rails-root))
                (progn
                  (add-hook 'local-write-file-hooks
                            '(lambda()
                               (save-excursion
                                 (untabify (point-min) (point-max))
                                 (delete-trailing-whitespace))))

                  (rails-minor-mode t)
                  (rails-erb-abbrev-init)
                  (if rails-use-another-define-key
                      (local-set-key "TAB"
                                     '(lambda() (interactive)
                                        (if snippet
                                            (snippet-next-field)
                                          (if (looking-at "\\>")
                                              (hippie-expand nil)
                                            (indent-for-tab-command)))))
                    (local-set-key (kbd "<tab>")
                                   '(lambda() (interactive)
                                      (if snippet
                                          (snippet-next-field)
                                        (if (looking-at "\\>")
                                            (hippie-expand nil)
                                          (indent-for-tab-command))))))))))

(provide 'rails)