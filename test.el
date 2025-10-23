(display-message-or-buffer (cl-prin1-to-string (benchmark-run 1000000
  (setq a 1 b 2 c 3 d 4 e 5 f 6 g 7 h 9 i 10 j 11 k 12 l 13 m 14))))

(display-message-or-buffer (cl-prin1-to-string (benchmark-run 1000000
  (setq a 1)
  (setq b 2)
  (setq c 3)
  (setq d 4)
  (setq e 5)
  (setq f 1)
  (setq g 2)
  (setq h 3)
  (setq i 4)
  (setq l 5)
  (setq m 1))))
