(defvar zmonitor-port 31415
  "port to listen for ox logs on")

(defun zmonitor-start ()
  (interactive)
  (unless (process-status "zmonitor")
	(make-network-process :name "zmonitor" :buffer "*zmonitor*"
						  :server 't :family 'ipv4 :service zmonitor-port
						  :filter 'zmonitor-filter))
  (with-current-buffer "*zmonitor*"
	(read-only-mode)))

(defun zmonitor-stop ()
  (interactive)
  (if (process-status "zmonitor")
	  (delete-process "zmonitor")))

(defun zmonitor-filter (proc string)
  (if (get-buffer "*zmonitor*")
	  (append-to-buffer "*zmonitor*" string)))

(defun append-to-buffer (buffer string)
  (let ((win (get-buffer-window buffer))
		(inhibit-read-only t))
	(if (eq win nil)
		(with-current-buffer buffer
		  (goto-char (point-max))
		  (insert string))
	  (with-selected-window
		  (get-buffer-window buffer)
		(goto-char (point-max))
		(insert string)))))
