(require "vm.lisp")
(defun runtest ()
    (progn
    (make-vm)
    (vm-load 'vm '((MOVE 5 :R1) (MOVE 3 :R2) (ADD :R1 :R2)))
    (vm-print)
    )

)