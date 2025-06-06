    [org    0x0100]                       
    jmp     start                         



    message: db 'SCORE', 0                  ; null terminated string

    message1: db 'HI-SCORE', 0               ; null terminated string

    message2: db 'LEVEL', 0                  ; null terminated string

    message3: db '1', 0                      ; null terminated string

    message4: db 'Time', 0                   ; null terminated string

    message5: db 'Upcoming Shape', 0         ; null terminated string

    message6: db 'The game is over', 0       ; null terminated string

    message7: db 'Congratratulations!', 0    ; null terminated string

    message8: db 'You scored: ', 0           ; null terminated string

    message9: db 'T E T R I S', 0            ; null terminated string


    oldisroftimer: dd 0                          

    tickcount: dw 0                          

    secondcount: dw 0                          

    minutecount: dw 0                          

    shiftleft: dw 0                          

    shiftright: dw 0                          

    oldisrofkeyboard:dd 0                             

    currentshape: dw 0                          

    currentrow: dw 0                          

    currentcol: dw 0                          

    initialrow:dw 0                             

    initialcol:dw 0                             

    randNum: dw 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

    randSeed: dw 0x1234                     

    num:    dw 0                          

    counter: dw 0                          

    cannotmove: dw 0                          

    seed:   dw 0                          

    count:  dw 0                          

    TouchedDown: dw 0                          

    score:  dw 0                          




beep:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            
    push    cx                            
    push    dx                            


    mov     al, 182                        ; Prepare the speaker for the
    out     43h, al                        ; note.
    mov     ax, [bp+4]                     ; Frequency number (in decimal)
    ;  for middle C
    out     42h, al                        ; Output low byte.
    mov     al, ah                         ; Output high byte.
    out     42h, al                       
    in      al, 61h                        ; Turn on note (get value from
    ;  port 61h).
    or      al, 00000011b                  ; Set bits 1 and 0.
    out     61h, al                        ; Send new value.
    mov     bx, [bp+6]                     ; Pause for duration of note.
pause1:
    mov     cx, 6535                      
pause2:
    dec     cx                            
    jne     pause2                        
    dec     bx                            
    jne     pause1                        
    in      al, 61h                        ; Turn off note (get value from
    ;  port 61h).
    and     al, 11111100b                  ; Reset bits 1 and 0.
    out     61h, al                        ; Send new value.

    pop     dx                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret     4                             


    ; subroutine to calculate the length of a string
    ; takes the segment and offset of a string as parameters
    strlen: push bp                       
    mov     bp,sp                         
    push    es                            
    push    cx                            
    push    di                            
    les     di, [bp+4]                     ; point es:di to string
    mov     cx, 0xffff                     ; load maximum number in cx
    xor     al, al                         ; load a zero in al
    repne   scasb                          ; find zero in the string
    mov     ax, 0xffff                     ; load maximum number in ax
    sub     ax, cx                         ; find change in cx
    dec     ax                             ; exclude null from length
    pop     di                            
    pop     cx                            
    pop     es                            
    pop     bp                            
    ret     4                             


    ; subroutine to print a string
    ; takes the x position, y position, attribute, and address of a null
    ; terminated string as parameters
    printstr: push bp                       
    mov     bp, sp                        
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    ds                             ; push segment of string
    mov     ax, [bp+4]                    
    push    ax                             ; push offset of string
    call    strlen                         ; calculate string length
    cmp     ax, 0                          ; is the string empty
    jz      exit                           ; no printing if string is empty
    mov     cx, ax                         ; save length in cx
    mov     ax, 0xb800                    
    mov     es, ax                         ; point es to video base
    mov     al, 80                         ; load al with columns per row
    mul     byte [bp+8]                    ; multiply with y position
    add     ax, [bp+10]                    ; add x position
    shl     ax, 1                          ; turn into byte offset
    mov     di,ax                          ; point di to required location
    mov     si, [bp+4]                     ; point si to string
    mov     ah, [bp+6]                     ; load attribute in ah
    cld                                   ; auto increment mode
    nextchar: lodsb                          ; load next char in al
    stosw                                 ; print char/attribute pair
    loop    nextchar                       ; repeat for the whole string
exit:
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             





PrintNum:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    bx                            
    push    cx                            
    push    dx                            
    push    di                            




    ;Pushing the integer digit by digit on stack

    mov     ax,[bp+4]                     
    mov     bx,10                         
    mov     cx,0                          

NextDigit:
    mov     dx,0                          
    div     bx                            
    add     dx,0x30                        ; generating the ascii value of the number
    push    dx                            
    inc     cx                            
    cmp     ax,0                           ; seeing if the quotient became zero
    jnz     NextDigit                     

    ;making a standard of 6 digit printing
    mov     ax,6                           ; total No. of digits
    sub     ax,cx                         
    jz      MoveOn                        


AddZeros:
    mov     dx,0x30                        ; Ascii value of Hex of 0
    push    dx                            
    inc     cx                            
    sub     ax,1                          
    jnz     AddZeros                      

MoveOn:
    ;printing the integer on screen

    cld                                  
    mov     ax,0xb800                     
    mov     es,ax                         
    mov     al,80                         
    mul     byte[bp+8]                    
    add     ax,[bp+10]                    
    shl     ax,1                          
    mov     di,ax                         


NextPosition:
    pop     ax                            
    mov     ah,[bp+6]                     
    stosw                                
    loop    NextPosition                  

    pop     di                            
    pop     dx                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             



PrintTime:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    bx                            
    push    cx                            
    push    dx                            
    push    di                            


    ;First we move seconds onto stack

    ;Pushing the integer digit by digit on stack

    mov     ax,[bp+4]                     
    mov     bx,10                         
    mov     cx,0                          



NextDigit1:
    mov     dx,0                          
    div     bx                            
    add     dx,0x30                        ; generating the ascii value of the number
    push    dx                            
    inc     cx                            
    cmp     ax,0                           ; seeing if the quotient became zero
    jnz     NextDigit1                    

    ;making a standard of at least 2 digit printing
    mov     ax,2                           ; total No. of digits
    sub     ax,cx                         
    jz      MoveOn1                       


AddZeros1:
    mov     dx,0x30                        ; Ascii value of Hex of 0
    push    dx                            
    inc     cx                            
    sub     ax,1                          
    jnz     AddZeros1                     

MoveOn1:

    ;Now pushing : on stack
    mov     ax,':'                        
    push    ax                            
    inc     cx                            


    ;Now pushing Minutes on stack

    ;Pushing the integer digit by digit on stack

    mov     ax,[bp+6]                     
    mov     bx,10                         

NextDigit2:
    mov     dx,0                          
    div     bx                            
    add     dx,0x30                        ; generating the ascii value of the number
    push    dx                            
    inc     cx                            
    cmp     ax,0                           ; seeing if the quotient became zero
    jnz     NextDigit2                    

    ;making a standard of at least 2 digit printing
    ;Already 2 digits were of seconds then 1 was of : so cx at the moment was 3 when minutes were to be pushed on stack

    mov     ax,5                           ; total No. of digits
    sub     ax,cx                         
    jz      MoveOn2                       


AddZeros2:
    mov     dx,0x30                        ; Ascii value of Hex of 0
    push    dx                            
    inc     cx                            
    sub     ax,1                          
    jnz     AddZeros2                     

MoveOn2:



    ;printing the integer on screen

    cld                                  
    mov     ax,0xb800                     
    mov     es,ax                         
    mov     al,80                         
    mul     byte[bp+10]                   
    add     ax,[bp+12]                    
    shl     ax,1                          
    mov     di,ax                         


NextPosition2:
    pop     ax                            
    mov     ah,[bp+8]                     
    stosw                                
    loop    NextPosition2                 

    pop     di                            
    pop     dx                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     10                            



LinePrint:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            



    mov     ax,0xB800                     
    mov     es,ax                         

    mov     al,80                         
    mul     byte[bp+4]                     ; row multiplied by 80 and stored ax
    add     ax,[bp+6]                      ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         

    mov     cx,[bp+8]                      ; how many times stos will be repeated

    mov     ah,[bp+10]                     ; storing attribute
    mov     al,[bp+12]                     ; storing character

    rep     stosw                         

    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     10                            


ClearScreen:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    di                            

    xor     di,di                         
    mov     ax,0xb800                     
    mov     es,ax                         

    mov     ax,0x0720                     
    mov     cx,2000                       

    cld                                  
    rep     stosw                         

    pop     di                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret                                  


Rectangle:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            

    mov     dx,[bp+4]                     
    mov     ax,[bp+6]                     
    sub     dx,ax                          ; calculating how much lenth is of the line

    mov     cx,[bp+8]                     
    mov     ax,[bp+10]                    
    sub     cx,ax                          ; calculating the width of the line

    mov     si,[bp+10]                     ; the row to start with

l1:
    mov     ax,[bp+14]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+12]                    
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    l1                            


    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     12                            


Shape1:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,4                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,2                          

loop3:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop3                         


    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             

Shape2:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,2                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,4                          

loop4:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop4                         


    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             


RANDGEN1:

    push    bp                            
    mov     bp, sp                        
    push    cx                            
    push    dx                            
    push    ax                            
    rdtsc                                 ; getting a random number in ax dx
    xor     dx,dx                          ; making dx 0
    mov     cx, 5                         
    div     cx                             ; dividing by 'Paramter' to get numbers from 0 - Parameter
    add     dx,1                          
    mov     [randNum], dx                  ; moving the random number in variable
    pop     ax                            
    pop     dx                            
    pop     cx                            
    pop     bp                            

    ret                                  

GenRandNum:
    push    bp                            
    mov     bp, sp                        
    push    cx                            
    push    ax                            
    push    dx                            

    MOV     AH, 00h                       
    INT     1AH                            ; Get system time (CX:DX now holds clock ticks since midnight)

    mov     ax, dx                        
    xor     dx, dx                        
    mov     cx, 5                          ; Change the divisor to 5 for numbers from 0 to 4
    div     cx                             ; Calculate remainder of clock ticks divided by 5

    add     dx,1                          
    mov     word [randNum], dx             ; Store the result in the memory location labeled randNum


    mov     ax, 44                        
    push    ax                             ; push x position
    mov     ax, 14                        
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; white foreground black background
    push    ax                             ; push attribute
    mov     ax, [randNum]                 
    push    ax                             ; integer to be printed
    call    PrintNum                      

    pop     dx                            
    pop     ax                            
    pop     cx                            
    pop     bp                            
    ret                                  



checkDown:

    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            


    ;calculating the position of current shape
    mov     ax,0xB800                     
    mov     es,ax                         

    mov     ax,80                         
    mul     word[currentrow]               ; row multiplied by 80 and stored ax
    add     ax,[currentcol]                ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         
    mov     si,ax                         

    cmp     word[currentshape],1          
    jne     fig2                           ; if not equal go to next shape removal

    ;checking if it has reached lower boundary

    cmp     word[currentrow],19           
    jne     lk                            
    mov     word[TouchedDown],1           
    jmp     fig6                          

lk:

    ;checking if there is a shape beneath it

    add     di,640                         ; bottom of shape 1
    mov     cx,2                          


    mov     ax,0x0720                      ; storing attribute black


la1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lb1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

lb1:

    add     di,2                          

    loop    la1                           



    mov     word[TouchedDown],0           

    jmp     fig6                          


fig2:

    cmp     word[currentshape],2          
    jne     fig3                           ; if not equal go to next shape removal


    cmp     word[currentrow],21           
    jne     ll                            

    mov     word[TouchedDown],1           
    jmp     fig6                          

ll:


    ;checking if there is a shape beneath it
    add     di,320                         ; bottom of shape 2
    mov     cx,4                          


    mov     ax,0x0720                      ; storing space character with black attribute


lc1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      ld1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

ld1:

    add     di,2                          

    loop    lc1                           


    mov     word[TouchedDown],0           


    jmp     fig6                          

fig3:

    cmp     word[currentshape],3          
    jne     fig4                           ; if not equal go to next shape removal



    cmp     word[currentrow],21           
    jne     lm                            
    mov     word[TouchedDown],1           

    jmp     fig6                          
lm:


    ;checking if there is a shape beneath it
    add     di,320                         ; bottom of shape 2
    mov     cx,6                          


    mov     ax,0x0720                      ; storing space character with black attribute


le1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lf1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

lf1:

    add     di,2                          

    loop    le1                           



    mov     word[TouchedDown],0           

    jmp     fig6                          

fig4:

    cmp     word[currentshape],4          
    jne     fig5                           ; if not equal go to next shape removal



    cmp     word[currentrow],21           
    jne     ln                            

    mov     word[TouchedDown],1           
    jmp     fig6                          
ln:


    ;checking if there is a shape beneath it
    add     di,320                         ; bottom of shape 2
    mov     cx,6                          


    mov     ax,0x0720                      ; storing space character with black attribute


lg1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lh1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

lh1:

    add     di,2                          

    loop    lg1                           



    mov     word[TouchedDown],0           

    jmp     fig6                          


fig5:


    cmp     word[currentshape],5          
    jne     fig6                           ; if not equal go to next shape removal



    cmp     word[currentrow],21           
    jne     lo                            

    mov     word[TouchedDown],1           
    jmp     fig6                          

lo:


    ;checking if there is a shape beneath it
    ;This has two separate portions one is of checking the bottom of first 2 down blocks then the last one
    ;Portion 1

    ;Portion 2
    add     si,168                         ; depending on complex geometry of this shape wherby we have to check the upper right block downward as well
    mov     ax,0x0720                      ; storing space character with black attribute

    mov     cx,2                          

lk1:
    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      ll1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

ll1:

    add     si,2                          
    loop    lk1                           



    mov     word[TouchedDown],0           


    add     di,320                         ; bottom of shape 2
    mov     cx,4                          


    mov     ax,0x0720                      ; storing space character with black attribute


li1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lj1                           


    mov     word[TouchedDown],1           
    jmp     fig6                          

lj1:

    add     di,2                          

    loop    li1                           






    mov     word[TouchedDown],0           


fig6:




    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     bp                            
    ret                                  

ScanFirstRow:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            
    push    cx                            
    push    si                            
    push    di                            

    ;here we calculated the exact location of the first row and first column
    mov     bl,3                           ; 3rd row is the first one
    mov     ax,80                         
    mul     bl                             ; row multiplied by 80 and stored ax
    add     ax,8                           ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         


    mov     ax,0xb800                     
    mov     es,ax                         
    mov     ax,0x0720                     

    mov     cx,34                          ; total number of cols to scan
    repe    scasw                         

    cmp     cx,0                          
    jz      exit6                         


    mov     word[minutecount],5           


exit6:
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret                                  

checkIfNewShapeStacked:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            


    cmp     word[currentcol],24           
    jne     exit7                         

    cmp     word[currentrow],3            
    jne     exit7                         

    cmp     word[TouchedDown],1           
    jne     exit7                         

    mov     word[minutecount],5           


exit7:
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret                                  




MinuteDelayTimer:
    cmp     word [minutecount],5          
    jne     MinuteDelayTimer              
    ret                                  






timer:
    push    ax                            
    push    bx                            

    inc     word [cs:tickcount]           
    mov     ax,word [cs:tickcount]        

    cmp     ax,18                         
    je      ly                             ; Here no change in seconds or minute so jump to l1
    jmp     loop15                        


ly:
    inc     word [cs:secondcount]          ; if there is an increase then increase the seconds count
    mov     word [cs:tickcount],0          ; again tick count will start counting
    cmp     word [cs:secondcount],60       ; if seconds have reached 60 add 1 in the minute count and reset seconds to 0
    jne     loop16                        

    inc     word [cs:minutecount]         
    mov     word [cs:secondcount],0       

loop16:
    ;Printing value of time
    mov     ax, 50                        
    push    ax                             ; column
    mov     ax, 24                        
    push    ax                             ; row
    mov     ax, 0x0F                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax,word [cs:minutecount]       ; Minutes
    push    ax                            
    mov     ax,word [cs:secondcount]       ; Seconds
    push    ax                            
    call    PrintTime                     



    ;Removing the shape and reprinting

    call    checkDown                     

    call    checkIfNewShapeStacked        

    cmp     word[TouchedDown],1           
    jne     l17                           

    cmp     word[cs:count],1              
    je      lz                            

    mov     word[cs:count],1              
    jmp     loop15                        

lz:
    mov     word[cs:count],0              
    mov     word[cs:TouchedDown],0        
    call    scanScreen                    

    call    GenNewShape                   

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     
    jmp     loop15                        

    mov     ax,18                          ; adjusting loop
    push    ax                            
    mov     ax,5000                        ; frequency
    push    ax                            
    call    beep                          

l17:
    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    RemoveShape                   

    inc     word [cs:currentrow]          

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     


    mov     ax,18                          ; adjusting loop
    push    ax                            
    mov     ax,5000                        ; frequency
    push    ax                            
    call    beep                          

    call    ScanFirstRow                  

loop15:

    mov     al,0x20                       
    out     0x20,al                       

    pop     ax                            
    pop     bx                            

    iret                                 


GenNewShape:


    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    di                            




    ;making upcoming shape the current one


    mov     ax,word[cs:randNum]           
    mov     word[cs:currentshape],ax      
    mov     ax,word[cs:initialrow]        
    mov     word[cs:currentrow],ax        
    mov     ax,word[cs:initialcol]        
    mov     word[cs:currentcol],ax        



    ;removing the previous upcoming shape
    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,52                         
    push    ax                            
    mov     ax,11                         
    push    ax                            
    call    RemoveShape                   



    ;Printing upcoming shape
    add     word [cs:num],2               
    call    RANDGEN1                      


    mov     di,[cs:num]                   
    ;Printing upcoming shape
    mov     ax,[cs:randNum]               
    push    ax                            
    mov     ax,52                         
    push    ax                            
    mov     ax,11                         
    push    ax                            
    call    MakeShape                     


    pop     di                            
    pop     ax                            
    pop     bp                            
    ret                                  



Shape3:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,6                           ; # of cols

loop55:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop55                        

    add     cx,1                          
    mov     dx,2                          
    sub     si,1                          

loop56:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    add     ax,4                          
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop56                        

    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             


Shape33:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,6                           ; # of cols

loop551:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop551                       



    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             

Shape35:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,10                          ; # of cols

loop555:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop555                       



    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             


Shape34:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,2                           ; # of cols

loop552:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop552                       



    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             

Shape4:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,6                           ; # of cols

loop57:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop57                        

    add     cx,1                          
    mov     dx,2                          
    sub     si,1                          

loop58:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    add     ax,2                          
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop58                        

    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             



Shape5:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     cx,1                           ; 8 rows are to be printed of width 2
    mov     si,[bp+4]                      ; the row to start with
    mov     dx,4                           ; # of cols

loop61:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    push    ax                            
    mov     ax,si                          ; starting  row
    add     ax,1                          
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop61                        

    add     cx,1                          
    mov     dx,4                          
    sub     si,1                          

loop62:
    mov     ax,[bp+10]                    
    push    ax                             ; character to be printed
    mov     ax,[bp+8]                     
    push    ax                             ; attribute byte
    push    dx                             ; length of printing
    mov     ax,[bp+6]                      ; starting column which will always remain the same
    add     ax,2                          
    push    ax                            
    mov     ax,si                          ; starting  row
    push    ax                            
    call    LinePrint                     

    add     si,1                          
    loop    loop62                        

    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret     8                             



MakeShape:

    push    bp                            
    mov     bp,sp                         
    push    ax                            

    cmp     word[bp+8],1                  
    jne     sha2                          

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape1                        
    jmp     sha6                          

sha2:


    cmp     word[bp+8],2                  
    jne     sha3                           ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape2                        
    jmp     sha6                          

sha3:

    cmp     word[bp+8],3                  
    jne     sha4                           ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape3                        
    jmp     sha6                          

sha4:

    cmp     word[bp+8],4                  
    jne     sha5                           ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape4                        
    jmp     sha6                          

sha5:


    cmp     word[bp+8],5                  
    jne     sha6                           ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape5                        


sha6:

    pop     ax                            
    pop     bp                            
    ret     6                             






RemoveShape:
    push    bp                            
    mov     bp,sp                         
    push    ax                            

    cmp     word[bp+8],1                  
    jne     sh2                            ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,7                           ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape1                        

    jmp     sh6                           

sh2:


    cmp     word[bp+8],2                  
    jne     sh3                            ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,7                           ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape2                        
    jmp     sh6                           

sh3:

    cmp     word[bp+8],3                  
    jne     sh4                            ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,7                           ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape3                        
    jmp     sh6                           

sh4:

    cmp     word[bp+8],4                  
    jne     sh5                            ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,7                           ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape4                        
    jmp     sh6                           

sh5:


    cmp     word[bp+8],5                  
    jne     sh6                            ; if not equal go to next shape removal

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,7                           ; attribute byte
    push    ax                            
    mov     ax,[bp+6]                      ; column
    push    ax                            
    mov     ax,[bp+4]                      ; row
    push    ax                            
    call    Shape5                        


sh6:

    pop     ax                            
    pop     bp                            
    ret     6                             


checkRight:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     ax,0xB800                     
    mov     es,ax                         

    mov     ax,80                         
    mul     word[currentrow]               ; row multiplied by 80 and stored ax
    add     ax,[currentcol]                ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         
    mov     si,ax                         


    cmp     word[currentshape],1          
    jne     shap2                          ; if not equal go to next shape removal


    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],40           
    jne     lf                            
    mov     word[cannotmove],1            
    jmp     shap6                         

lf:
    ;checking if there is a shape on right side

    add     di,4                           ; the position exactly right side of shape for attribute byte checking

    mov     cx,4                          

    mov     ax,0x0720                      ; storing attribute black


lq:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lr                            

    mov     word[cannotmove],1            
    jmp     shap6                         

lr:

    add     di,160                        

    loop    lq                            


    jmp     shap6                         

shap2:


    cmp     word[currentshape],2          
    jne     shap3                          ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],38           
    jne     lg                            
    mov     word[cannotmove],1            
    jmp     shap6                         

lg:


    ;checking if there is a shape on right side

    add     di,8                           ; the position exactly right side of shape for attribute byte checking

    mov     cx,2                          

    mov     ax,0x0720                      ; storing attribute black


lm1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      ln1                           

    mov     word[cannotmove],1            
    jmp     shap6                         

ln1:

    add     di,160                        

    loop    lm1                           



    jmp     shap6                         

shap3:

    cmp     word[currentshape],3          
    jne     shap4                          ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],36           
    jne     lh                            
    mov     word[cannotmove],1            
    jmp     shap6                         

lh:


    ;checking if there is a shape on right side

    add     di,12                          ; the position exactly right side of shape for attribute byte checking

    mov     cx,2                          

    mov     ax,0x0720                      ; storing attribute black


lo1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lp1                           

    mov     word[cannotmove],1            
    jmp     shap6                         

lp1:

    add     di,160                        

    loop    lo1                           



    jmp     shap6                         

shap4:

    cmp     word[currentshape],4          
    jne     shap5                          ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],36           
    jne     li                            
    mov     word[cannotmove],1            
    jmp     shap6                         

li:



    ;checking if there is a shape on right side



    add     di,160                         ; next line
    add     di,12                          ; go to the point to check if someone is adjacent to lower row

    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program
    ;we have to check once no loop

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lr1                           

    mov     word[cannotmove],1            
    jmp     shap6                         

lr1:



    add     si,8                           ; seeing the starting line right side to the central block


    ;we have to check once so no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program


    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      lq1                           

    mov     word[cannotmove],1            

lq1:



    jmp     shap6                         

shap5:


    cmp     word[currentshape],5          
    jne     shap6                          ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],36           
    jne     lj                            
    mov     word[cannotmove],1            
    jmp     shap6                         


lj:


    ;checking if there is a shape on right side



    add     si,12                          ; seeing the starting line right side to the blocks

    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program
    ;we have to check once so no loop

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      ls1                           

    mov     word[cannotmove],1            
    jmp     shap6                         

ls1:



    add     di,160                         ; next line
    add     di,8                           ; go to the point to check if someone is adjacent to lower row

    ;we have to check once no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program


    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lt1                           

    mov     word[cannotmove],1            


lt1:



shap6:




    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret                                  



checkLeft:
    push    bp                            
    mov     bp,sp                         
    push    es                            
    push    ax                            
    push    cx                            
    push    si                            
    push    di                            
    push    dx                            



    mov     ax,0xB800                     
    mov     es,ax                         

    mov     ax,80                         
    mul     word[currentrow]               ; row multiplied by 80 and stored ax
    add     ax,[currentcol]                ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         
    mov     si,ax                         



    cmp     word[currentshape],1          
    jne     s2                             ; if not equal go to next shape removal


    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],8            
    jne     la                            
    mov     word[cannotmove],1            
    jmp     s6                            

la:

    ;checking if there is a shape on left side

    sub     di,4                           ; the position exactly left side of shape for attribute byte checking

    mov     cx,4                          

    mov     ax,0x0720                      ; storing attribute black


lu1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lv1                           

    mov     word[cannotmove],1            
    jmp     s6                            

lv1:

    add     di,160                        

    loop    lu1                           


    jmp     s6                            

s2:


    cmp     word[currentshape],2          
    jne     s3                             ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],8            
    jne     lb                            
    mov     word[cannotmove],1            
    jmp     s6                            
lb:

    ;checking if there is a shape on left side

    sub     di,4                           ; the position exactly left side of shape for attribute byte checking

    mov     cx,2                          

    mov     ax,0x0720                      ; storing attribute black


lw1:
    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lx1                           

    mov     word[cannotmove],1            
    jmp     s6                            

lx1:

    add     di,160                        

    loop    lw1                           

    jmp     s6                            

s3:

    cmp     word[currentshape],3          
    jne     s4                             ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],8            
    jne     lc                            
    mov     word[cannotmove],1            
    jmp     s6                            
lc:


    ;checking if there is a shape on left side



    add     si,4                           ; seeing the starting line left side to the rightmost block


    ;we have to check once so no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      ly1                           

    mov     word[cannotmove],1            
    jmp     s6                            

ly1:



    add     di,160                         ; next line
    sub     di,4                           ; go to the point to check if someone is adjacent to lower row

    ;we have to check once no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program to check if some other block is present there

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lz1                           

    mov     word[cannotmove],1            


lz1:



    jmp     s6                            

s4:

    cmp     word[currentshape],4          
    jne     s5                             ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],8            
    jne     ld                            
    mov     word[cannotmove],1            
    jmp     s6                            
ld:


    ;checking if there is a shape on left side



    ;Here SI is perfectly fine at the location which we have to check in first row for any other object


    ;we have to check once so no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      la2                           

    mov     word[cannotmove],1            
    jmp     s6                            

la2:



    add     di,160                         ; next line
    sub     di,4                           ; go to the point to check if someone is adjacent to lower row

    ;we have to check once no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program to check if some other block is present there

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      lb2                           

    mov     word[cannotmove],1            


lb2:





    jmp     s6                            

s5:


    cmp     word[currentshape],5          
    jne     s6                             ; if not equal go to next shape removal



    ;no figure can go on the left side of column no of 8 it is the fundamental boundary
    cmp     word[currentcol],8            
    jne     le                            
    mov     word[cannotmove],1            
    jmp     s6                            
le:



    ;checking if there is a shape on left side



    ;Here SI is perfectly fine at the location which we have to check in first row for any other object


    ;we have to check once so no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:si]                
    cmp     ax,bx                         
    je      lc2                           

    mov     word[cannotmove],1            
    jmp     s6                            

lc2:



    add     di,160                         ; next line
    sub     di,4                           ; go to the point to check if someone is adjacent to lower row

    ;we have to check once no loop
    ;Remember that we only need to check one of the 2 columns of a block thats enough in our program to check if some other block is present there

    mov     ax,0x0720                      ; storing attribute black


    mov     bx,word[es:di]                
    cmp     ax,bx                         
    je      ld2                           

    mov     word[cannotmove],1            


ld2:


s6:



    pop     dx                            
    pop     di                            
    pop     si                            
    pop     cx                            
    pop     ax                            
    pop     es                            
    pop     bp                            
    ret                                  







myisrofkeyboard:
    push    ax                            

    in      al,0x60                        ; reading a character from keyboard port
    cmp     al,0x4D                        ; right arrow key
    jne     nextcmp                       

    call    checkRight                    

    cmp     word[cannotmove],0            
    jne     nextcmp                       


    mov     ax,18                          ; adjusting loop
    push    ax                            
    mov     ax,7000                        ; frequency
    push    ax                            
    call    beep                          

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    RemoveShape                   

    add     word [cs:currentcol],2        

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     




nextcmp:
    in      al,0x60                       
    cmp     al,0x4B                        ; left arrow key
    jne     nextcmp1                      


    call    checkLeft                     

    cmp     word[cannotmove],0            
    jne     nextcmp1                      


    mov     ax,18                          ; adjusting loop
    push    ax                            
    mov     ax,7000                        ; frequency
    push    ax                            
    call    beep                          

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    RemoveShape                   

    sub     word [cs:currentcol],2        

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     



nextcmp1:
    in      al,0x60                       
    cmp     al,0x50                       
    jne     nomatch                       

    call    checkDown                     

    cmp     word[TouchedDown],1           
    je      l177                          


    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    RemoveShape                   

    inc     word [cs:currentrow]          

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     


l177:

    mov     word[TouchedDown],0           

    ;call GenNewShape

    ;mov ax,[cs:currentshape]
    ;push ax
    ;mov ax,[cs:currentcol]
    ;push ax
    ;mov ax,[cs:currentrow]
    ;push ax
    ;call MakeShape



    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     



nomatch:
    mov     word[cannotmove],0             ; ensuring that cannotmove is again set to 0 if it was 1

    pop     ax                            
    jmp     far [cs:oldisrofkeyboard]     




scanRow:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            
    push    cx                            
    push    dx                            
    push    es                            
    push    di                            

scanAgain:


    mov     al,80                         
    mul     byte[bp+4]                    
    add     ax,8                           ; starting column
    shl     ax,1                          
    mov     di,ax                         


    mov     ax,0xb800                     
    mov     es,ax                         
    mov     ax,0x0720                     

    mov     cx,35                          ; total number of cols
    repne   scasw                         

    cmp     cx,0                          
    jnz     exit5                         

    add     word[score],10                

    ;Printing the value of Score
    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 3                         
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, word[score]               
    push    ax                             ; integer to be printed
    call    PrintNum                      


    mov     ax,160                         ; adjusting loop
    push    ax                            
    mov     ax,9135                        ; frequency
    push    ax                            
    call    beep                          

    mov     ax,[bp+4]                     
    push    ax                            
    call    clearRow                      
    jmp     scanAgain                     

exit5:
    pop     di                            
    pop     es                            
    pop     dx                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret     2                             

clearRow:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            
    push    cx                            
    push    dx                            
    push    es                            
    push    di                            
    push    si                            
    push    ds                            


    ;first of all setting di to the row that needs to be cleared.

    mov     ax,80                         
    mul     byte[bp+4]                     ; row multiplied by 80 and stored ax
    add     ax,8                           ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         

    mov     si,di                         
    sub     si,160                        


    ;for movs
    mov     ax,0xb800                     
    mov     es,ax                         
    mov     ds,ax                         


    ;dx contains no of rows-1
    mov     dx,19                         

    ;ax contains the last row number
    mov     ax,22                         


    ;The remaining figure after we subtract the row we need to clear from the last row
    sub     ax,[bp+4]                     


    ;now dx will store how many rows we have to reprint downwards
    sub     dx,ax                         

    cld                                  

loop201:
    mov     ax,si                         

    mov     cx,35                          ; no of columns in a row
    rep     movsw                         

    ;now moving both di and si to one row up respectively
    mov     di,ax                         

    mov     si,di                         
    sub     si,160                        

    dec     dx                            
    jnz     loop201                       

    ;clearing the first row

    ;here we calculated the exact location of the first row and first column
    mov     bl,3                           ; 3rd row is the first one
    mov     ax,80                         
    mul     bl                             ; row multiplied by 80 and stored ax
    add     ax,8                           ; add column
    shl     ax,1                           ; multiplied by 2
    mov     di,ax                         


    mov     ax,0x0720                     
    mov     cx,34                         
    rep     stosw                         

    pop     ds                            
    pop     si                            
    pop     di                            
    pop     es                            
    pop     dx                            
    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret     2                             


scanScreen:
    push    bp                            
    mov     bp,sp                         
    push    ax                            
    push    bx                            
    push    cx                            

    mov     cx,22                          ; Moving the last row

loop200:
    push    cx                            
    call    scanRow                       
    dec     cx                            
    cmp     cx,2                           ; Comaparing with the first row
    jne     loop200                       

    pop     cx                            
    pop     bx                            
    pop     ax                            
    pop     bp                            
    ret                                  




Delay:
    mov     ax,0xffff                     
loop202:
    mov     cx,5                          

loop203:
    dec     cx                            
    jnz     loop203                       

    sub     ax,1                          
    jnz     loop202                       
    ret                                  

Delay1:
    mov     ax,0xffff                     
loop205:
    mov     cx,4                          

loop206:
    dec     cx                            
    jnz     loop206                       

    sub     ax,1                          
    jnz     loop205                       
    ret                                  


StartingScreen:
    push    ax                            


    call    ClearScreen                   

    ;upper blue

    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X1F                        ; attribute byte
    push    ax                            
    mov     ax,1                           ; top
    push    ax                            
    mov     ax,12                          ; bottom
    push    ax                            
    mov     ax,0                           ; left
    push    ax                            
    mov     ax,80                          ; right
    push    ax                            
    call    Rectangle                     

    ; upper black

    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X07                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; top
    push    ax                            
    mov     ax,11                          ; bottom
    push    ax                            
    mov     ax,2                           ; left
    push    ax                            
    mov     ax,78                          ; right
    push    ax                            
    call    Rectangle                     


    ;lower blue

    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X1F                        ; attribute byte
    push    ax                            
    mov     ax,12                          ; top
    push    ax                            
    mov     ax,22                          ; bottom
    push    ax                            
    mov     ax,24                          ; left
    push    ax                            
    mov     ax,54                          ; right
    push    ax                            
    call    Rectangle                     

    ;lower black

    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X07                        ; attribute byte
    push    ax                            
    mov     ax,13                          ; top
    push    ax                            
    mov     ax,21                          ; bottom
    push    ax                            
    mov     ax,26                          ; left
    push    ax                            
    mov     ax,52                          ; right
    push    ax                            
    call    Rectangle                     

    ;Central Black

    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X07                        ; attribute byte
    push    ax                            
    mov     ax,11                          ; top
    push    ax                            
    mov     ax,13                          ; bottom
    push    ax                            
    mov     ax,26                          ; left
    push    ax                            
    mov     ax,52                          ; right
    push    ax                            
    call    Rectangle                     


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    call    Delay1                        


    ;printing t

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,8                           ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,8                           ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,6                           ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       

    ;printing loading line

    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,10                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       


    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        

    ;printing E
    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,20                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       


    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        

    ;Printing T

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,32                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,32                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        

    ;printing R
    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,42                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,42                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,46                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,42                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,42                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,44                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape34                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,45                          ; column
    push    ax                            
    mov     ax,7                           ; row
    push    ax                            
    call    Shape34                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,46                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape34                       


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,40                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        

    ;Printing I

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,56                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,56                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,50                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        

    ;printing S
    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,66                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,70                          ; column
    push    ax                            
    mov     ax,6                           ; row
    push    ax                            
    call    Shape1                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,66                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,66                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,66                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,60                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    mov     ax,60                          ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay1                        


    ;Printing the shape itself
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,70                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape35                       

    mov     ax,180                         ; adjusting loop
    push    ax                            
    mov     ax,1200                        ; frequency
    push    ax                            
    call    beep                          

    call    Delay                         


    pop     ax                            
    ret                                  

PlayingScreen:
    push    ax                            


    call    ClearScreen                   




    ;Outer Rectangle
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0X50                        ; attribute byte
    push    ax                            
    mov     ax,1                           ; top
    push    ax                            
    mov     ax,25                          ; bottom
    push    ax                            
    mov     ax,4                           ; left
    push    ax                            
    mov     ax,46                          ; right
    push    ax                            
    call    Rectangle                     


    ;Inner Rectangle
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; top
    push    ax                            
    mov     ax,24                          ; bottom
    push    ax                            
    mov     ax,6                           ; left
    push    ax                            
    mov     ax,44                          ; right
    push    ax                            
    call    Rectangle                     

    ;Inner Rectangle
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x07                        ; attribute byte
    push    ax                            
    mov     ax,3                           ; top
    push    ax                            
    mov     ax,23                          ; bottom
    push    ax                            
    mov     ax,8                           ; left
    push    ax                            
    mov     ax,42                          ; right
    push    ax                            
    call    Rectangle                     


    ;Printing Score
    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 1                         
    push    ax                             ; push y position
    mov     ax, 0x0D                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message                   
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine

    ;Printing the value of Score
    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 3                         
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, word[score]               
    push    ax                             ; integer to be printed
    call    PrintNum                      





    ;Printing High-Score


    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 5                         
    push    ax                             ; push y position
    mov     ax, 0x0D                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message1                  
    push    ax                             ; push address of message c
    call    printstr                       ; call the printstr subroutine


    ;Printing the value of High-Score
    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 7                         
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax,160                         ; Pushing the value of high score
    push    ax                            
    call    PrintNum                      


    ;Printing message upcoming shape

    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 9                         
    push    ax                             ; push y position
    mov     ax, 0x0D                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message5                  
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine



    ;Printing level

    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 18                        
    push    ax                             ; push y position
    mov     ax, 0x0D                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message2                  
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine

    ;Printing value of level
    mov     ax, 52                        
    push    ax                             ; push x position
    mov     ax, 20                        
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message3                  
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine






    ;Printing the string timer

    mov     ax, 50                        
    push    ax                             ; push x position
    mov     ax, 22                        
    push    ax                             ; push y position
    mov     ax, 0x0D                       ; blue on white attribute
    push    ax                             ; push attribute
    mov     ax, message4                  
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine


    ;Printing the string Tetris

    mov     ax, 20                        
    push    ax                             ; push x position
    mov     ax, 1                         
    push    ax                             ; push y position
    mov     ax, 0x5F                       ; blue on pink attribute
    push    ax                             ; push attribute
    mov     ax, message9                  
    push    ax                             ; push address of message
    call    printstr                       ; call the printstr subroutine




    pop     ax                            
    ret                                  

EndScreen:

    call    ClearScreen                   

    ;printing G

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0X6F                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,6                           ; column
    push    ax                            
    mov     ax,4                           ; row
    push    ax                            
    call    Shape34                       

    call    Delay1                        

    ; printing A

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,10                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,10                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,14                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,10                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape33                       

    call    Delay1                        

    ;printing M

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x50                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x50                        ; attribute byte
    push    ax                            
    mov     ax,18                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x50                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x50                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x50                        ; attribute byte
    push    ax                            
    mov     ax,26                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    call    Delay1                        

    ;Printing E


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    call    Delay1                        

    ;printing o

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,44                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,44                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,44                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,48                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    call    Delay1                        


    ;printing v

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,52                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape1                        





    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,52                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,56                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape1                        

    call    Delay1                        


    ;Printing E


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,60                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,60                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,60                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,60                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape33                       

    call    Delay1                        


    ;printing R

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,1                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,3                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,72                          ; column
    push    ax                            
    mov     ax,2                           ; row
    push    ax                            
    call    Shape34                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,70                          ; column
    push    ax                            
    mov     ax,4                           ; row
    push    ax                            
    call    Shape34                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,72                          ; column
    push    ax                            
    mov     ax,5                           ; row
    push    ax                            
    call    Shape34                       

    call    Delay1                        

    ;printing score

    ;printing s




    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,12                          ; row
    push    ax                            
    call    Shape33                       

    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,22                          ; column
    push    ax                            
    mov     ax,9                           ; row
    push    ax                            
    call    Shape34                       

    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,26                          ; column
    push    ax                            
    mov     ax,11                          ; row
    push    ax                            
    call    Shape34                       

    ;printing c

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape1                        



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,30                          ; column
    push    ax                            
    mov     ax,12                          ; row
    push    ax                            
    call    Shape33                       

    ;printing o

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,38                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,42                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,38                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,38                          ; column
    push    ax                            
    mov     ax,12                          ; row
    push    ax                            
    call    Shape33                       

    ;printing R

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,46                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,46                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,46                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,50                          ; column
    push    ax                            
    mov     ax,9                           ; row
    push    ax                            
    call    Shape34                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,48                          ; column
    push    ax                            
    mov     ax,11                          ; row
    push    ax                            
    call    Shape34                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,50                          ; column
    push    ax                            
    mov     ax,12                          ; row
    push    ax                            
    call    Shape34                       

    ;Printing E


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,8                           ; row
    push    ax                            
    call    Shape33                       



    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,10                          ; row
    push    ax                            
    call    Shape33                       

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,54                          ; column
    push    ax                            
    mov     ax,12                          ; row
    push    ax                            
    call    Shape33                       




    ;Inner Rectangle
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x30                        ; attribute byte
    push    ax                            
    mov     ax,16                          ; top
    push    ax                            
    mov     ax,21                          ; bottom
    push    ax                            
    mov     ax,30                          ; left
    push    ax                            
    mov     ax,50                          ; right
    push    ax                            
    call    Rectangle                     

    ;Inner Rectangle
    mov     ax,' '                         ; character to be printed
    push    ax                            
    mov     ax,0x07                        ; attribute byte
    push    ax                            
    mov     ax,17                          ; top
    push    ax                            
    mov     ax,20                          ; bottom
    push    ax                            
    mov     ax,32                          ; left
    push    ax                            
    mov     ax,48                          ; right
    push    ax                            
    call    Rectangle                     



    ;Printing the numeric score
    mov     ax, 37                        
    push    ax                             ; push x position
    mov     ax, 18                        
    push    ax                             ; push y position
    mov     ax, 0x0F                       ; white foreground black background
    push    ax                             ; push attribute
    mov     ax, word[score]               
    push    ax                             ; integer to be printed
    call    PrintNum                      


    ;printing structure

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape2                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,76                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape2                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,6                           ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape3                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape3                        

    call    Delay1                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,6                           ; column
    push    ax                            
    mov     ax,22                          ; row
    push    ax                            
    call    Shape5                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,66                          ; column
    push    ax                            
    mov     ax,21                          ; row
    push    ax                            
    call    Shape1                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,21                          ; row
    push    ax                            
    call    Shape4                        

    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,58                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape4                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,18                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,62                          ; column
    push    ax                            
    mov     ax,22                          ; row
    push    ax                            
    call    Shape2                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,6                           ; column
    push    ax                            
    mov     ax,20                          ; row
    push    ax                            
    call    Shape3                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,22                          ; row
    push    ax                            
    call    Shape5                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,4                           ; column
    push    ax                            
    mov     ax,19                          ; row
    push    ax                            
    call    Shape2                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,74                          ; column
    push    ax                            
    mov     ax,21                          ; row
    push    ax                            
    call    Shape4                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,12                          ; column
    push    ax                            
    mov     ax,21                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,20                          ; row
    push    ax                            
    call    Shape3                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x5F                        ; attribute byte
    push    ax                            
    mov     ax,16                          ; column
    push    ax                            
    mov     ax,23                          ; row
    push    ax                            
    call    Shape4                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,74                          ; column
    push    ax                            
    mov     ax,19                          ; row
    push    ax                            
    call    Shape2                        

    call    Delay1                        


    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,14                          ; column
    push    ax                            
    mov     ax,22                          ; row
    push    ax                            
    call    Shape2                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,78                          ; column
    push    ax                            
    mov     ax,18                          ; row
    push    ax                            
    call    Shape1                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,2                           ; column
    push    ax                            
    mov     ax,17                          ; row
    push    ax                            
    call    Shape5                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x4F                        ; attribute byte
    push    ax                            
    mov     ax,72                          ; column
    push    ax                            
    mov     ax,17                          ; row
    push    ax                            
    call    Shape5                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,15                          ; row
    push    ax                            
    call    Shape3                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,68                          ; column
    push    ax                            
    mov     ax,19                          ; row
    push    ax                            
    call    Shape2                        

    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,0                           ; column
    push    ax                            
    mov     ax,14                          ; row
    push    ax                            
    call    Shape2                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x2E                        ; attribute byte
    push    ax                            
    mov     ax,74                          ; column
    push    ax                            
    mov     ax,15                          ; row
    push    ax                            
    call    Shape3                        


    call    Delay1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x1F                        ; attribute byte
    push    ax                            
    mov     ax,8                           ; column
    push    ax                            
    mov     ax,17                          ; row
    push    ax                            
    call    Shape1                        

    ;Printing the shape itself
    mov     ax,':'                         ; character to be printed
    push    ax                            
    mov     ax,0x6F                        ; attribute byte
    push    ax                            
    mov     ax,74                          ; column
    push    ax                            
    mov     ax,14                          ; row
    push    ax                            
    call    Shape2                        

    call    Delay1                        



    ret                                  


start:

    call    StartingScreen                
    call    PlayingScreen                 



    ;Setting the initial row and column

    mov     word[currentrow],3            
    mov     word[initialrow],3            

    mov     word [currentcol],24          
    mov     word [initialcol],24          





    ;The first shape is random
    call    RANDGEN1                      
    mov     ax,[randNum]                  
    mov     word [currentshape],ax        

    mov     ax,[cs:currentshape]          
    push    ax                            
    mov     ax,[cs:currentcol]            
    push    ax                            
    mov     ax,[cs:currentrow]            
    push    ax                            
    call    MakeShape                     


    ;Upcoming random shape calling
    call    RANDGEN1                      
    ;Printing upcoming shape
    mov     ax,[randNum]                  
    push    ax                            
    mov     ax,52                         
    push    ax                            
    mov     ax,11                         
    push    ax                            
    call    MakeShape                     




    ;Hooking the interupt of timer

    xor     ax,ax                         
    mov     es,ax                         

    ;Saving data for unhooking later on
    cli                                  
    mov     ax,[es:8*4]                   
    mov     [oldisroftimer],ax             ; saving offset of old subroutine
    mov     ax,[es:8*4+2]                 
    mov     [oldisroftimer+2],ax           ; saving segment of old subroutine
    sti                                  

    ;Hooking the interupt
    cli                                  
    mov     word [es:8*4],timer           
    mov     [es:8*4+2],cs                 
    sti                                  



    ;Hooking the interupt of keyboard

    xor     ax,ax                         
    mov     es,ax                         

    ;Saving data for unhooking later on
    cli                                  
    mov     ax,[es:9*4]                   
    mov     [oldisrofkeyboard],ax         
    mov     ax,[es:9*4+2]                 
    mov     [oldisrofkeyboard+2],ax       
    sti                                  

    ;Hooking the interupt
    cli                                  
    mov     word [es:9*4],myisrofkeyboard 
    mov     [es:9*4+2],cs                 
    sti                                  


    call    MinuteDelayTimer              


    ;Unhooking the interupt of timer

    xor     ax,ax                         
    mov     es,ax                         

    ;Unhooking the timer interupt
    mov     ax,[oldisroftimer]            
    mov     bx,[oldisroftimer+2]          
    cli                                  
    mov     [es:8*4],ax                   
    mov     [es:8*4+2],bx                 
    sti                                  

    ;Unhooking the interupt of keyboard

    xor     ax,ax                         
    mov     es,ax                         

    mov     ax,[oldisrofkeyboard]         
    mov     bx,[oldisrofkeyboard+2]       
    cli                                  
    mov     [es:9*4],ax                   
    mov     [es:9*4+2],bx                 
    sti                                  




    call    EndScreen                     


    mov     ax,0x4c00                     
    int     0x21                          
