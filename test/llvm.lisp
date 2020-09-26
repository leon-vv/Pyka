(load "src/llvm3.lisp")


(assert-equal
  (type->string 'i8)
  "i8")

(assert-equal
  (type->string '(pointer i8))
  "i8*")  

(assert-equal
  (type->string '(-> void (pointer i8)))
  "void (i8*)")

(assert-equal
  (type->string '(array 5 i8))
  "[5 x i8]")

(assert-equal
  (type->string '(struct i8 str))
  "{i8, str}")
    
(assert-equal
  (type-list->string '(i3 (pointer u16) (-> f16 str (array 5 i8))) "(" ")")
  "(i3, u16*, f16 (str, [5 x i8]))")

(assert-equal
  (arg-list->string '((i8 a) ((array 10 f64) b)) "(" ")")
  "(i8 a, [10 x f64] b)")

(assert-equal
  (add. '(pointer i8) 'a 'b)
  "add i8* a, b")

(assert-equal
  (declare. '(pointer i8) '@malloc '(i32))
  "declare i8* @malloc(i32)")

(assert-equal
  (fun. 'void '@String_Delete '( ((pointer %String) %this) (i8 %that) )
    (add. 'i32 'a 'b))
  "void @String_Delete(%String* %this, i8 %that) {\nadd i32 a, b\n}")

  

