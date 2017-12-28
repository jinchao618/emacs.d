(require-package 'company-ycmd)

(require 'company-ycmd)
(company-ycmd-setup)
(set-variable 'ycmd-server-command '("python2" "/home/jinchao/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))

(provide 'init-company)
