; ModuleID = 'main'


 


@putNumForm =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


declare external ccc  i32 @printf(i8*, ...)    


define external ccc  i32 @main()    {
entry_0:
  %0 = mul   i32 2, 3 
  %1 = sdiv  i32 %0, 4 
  %2 = add   i32 1, %1 
  %3 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @putNumForm, i32 0, i32 0), i32  %2)  
  ret i32 0 
}