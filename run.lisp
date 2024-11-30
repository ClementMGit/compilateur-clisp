(require "vm.lisp")
(defun run ()
    (progn
    (make-vm)
    (vm-load 'vm '((MOVE 5 :R1) (MOVE 3 :R2) (ADD :R1 :R2)))
    (vm-exec 'vm)
    (vm-print)
    )
)