((JMP #:|findefun4013|) (LABEL FIBO) (MOVE 0 :R0) (PUSH :R0) (LOAD (:FP -1) :R0) (PUSH :R0)
 (POP :R1) (POP :R0) (CMP :R0 :R1) (MOVE 1 :R0) (JEQ #:|finTest4016|) (MOVE 0 :R0)
 (LABEL #:|finTest4016|) (CMP :R0 0) (JEQ #:|else4014|) (MOVE 0 :R0) (JMP #:|endif4015|)
 (LABEL #:|else4014|) (MOVE 1 :R0) (PUSH :R0) (LOAD (:FP -1) :R0) (PUSH :R0) (POP :R1)
 (POP :R0) (CMP :R0 :R1) (MOVE 1 :R0) (JEQ #:|finTest4019|) (MOVE 0 :R0)
 (LABEL #:|finTest4019|) (CMP :R0 0) (JEQ #:|else4017|) (MOVE 1 :R0) (JMP #:|endif4018|)
 (LABEL #:|else4017|) (LOAD (:FP -1) :R0) (PUSH :R0) (MOVE 1 :R0) (PUSH :R0) (POP :R1)
 (POP :R0) (SUB :R1 :R0) (PUSH :R0) (MOVE :FP :R1) (MOVE :SP :FP) (PUSH 1) (MOVE :SP :R2)
 (SUB 2 :R2) (PUSH :R2) (PUSH :R1) (JSR FIBO) (POP :R1) (POP :R2) (MOVE :R1 :FP) (MOVE :R2 :SP)
 (PUSH :R0) (LOAD (:FP -1) :R0) (PUSH :R0) (MOVE 2 :R0) (PUSH :R0) (POP :R1) (POP :R0)
 (SUB :R1 :R0) (PUSH :R0) (MOVE :FP :R1) (MOVE :SP :FP) (PUSH 1) (MOVE :SP :R2) (SUB 2 :R2)
 (PUSH :R2) (PUSH :R1) (JSR FIBO) (POP :R1) (POP :R2) (MOVE :R1 :FP) (MOVE :R2 :SP) (PUSH :R0)
 (POP :R1) (POP :R0) (ADD :R1 :R0) (LABEL #:|endif4018|) (LABEL #:|endif4015|) (RTN)
 (LABEL #:|findefun4013|) (MOVE 10 :R0) (PUSH :R0) (MOVE :FP :R1) (MOVE :SP :FP) (PUSH 1)
 (MOVE :SP :R2) (SUB 2 :R2) (PUSH :R2) (PUSH :R1) (JSR FIBO) (POP :R1) (POP :R2) (MOVE :R1 :FP)
 (MOVE :R2 :SP))