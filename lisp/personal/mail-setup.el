
;;; Mail fetching is done by offlineimap, installed as brew package
;;; It is configured by
;;; ~/.offlineimaprc and has a LaunchAgent file in
;;; ~/Libary/LauchAgents/

;; febeling@flomac ~> brew install offlineimap
;; ==> Downloading https://github.com/OfflineIMAP/offlineimap/archive/v6.5.5.tar.gz
;; ######################################################################## 100.0%
;; ==> Caveats
;; To get started, copy one of these configurations to ~/.offlineimaprc:
;; * minimal configuration:
;;     cp -n /usr/local/Cellar/offline-imap/6.5.5/offlineimap.conf.minimal ~/.offlineimaprc

;; * advanced configuration:
;;     cp -n /usr/local/Cellar/offline-imap/6.5.5/offlineimap.conf ~/.offlineimaprc

;; To have launchd start offline-imap at login:
;;     ln -sfv /usr/local/opt/offline-imap/*.plist ~/Library/LaunchAgents
;; Then to load offline-imap now:
;;     launchctl load ~/Library/LaunchAgents/homebrew.mxcl.offline-imap.plist
;; ==> Summary
;; 🍺  /usr/local/Cellar/offline-imap/6.5.5: 46 files, 560K, built in 2 seconds

;; Sometimes, it can be useful to just queue the mails and send them
;; later all at once (dialup users, etc…), then just add:
;;(setq smtpmail-queue-mail t)
;; When your network connection is ready, just do
;; M-x smtpmail-send-queued-mail RET

;; needs to be specified before the (require)
;; (setq smtpmail-default-smtp-server "smtp.gmail.com")
;; (require 'smtpmail)
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (setq user-full-name "Florian Ebeling")
;; (setq smtpmail-local-domain "40lines.com")
;; (setq user-mail-address (concat "florian.ebeling@" smtpmail-local-domain))

(setq
 user-mail-address "florian.ebeling@40lines.com"
 user-full-name  "Florian Ebeling"
 ;; message-signature
 ;;  (concat
 ;;    "Foo X. Bar\n"
 ;;    "http://www.example.com\n")
)

(require 'smtpmail)

;;; see https://gist.github.com/areina/3879626

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg") ;; .gpg -> encrypt later
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)
