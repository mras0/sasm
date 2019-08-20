        cpu 8086

        call [es:bx+0x1234]  ; FF /2
        call 0x1234:0x5678   ; 9A ?
        call far [bp+0]      ; FF /3
        jmp bx               ; FF /4
        jmp [si]             ; FF /4
        jmp 0x1234:0x5678    ; EA
        jmp far [24]         ; FF /5
        push word [es:bx+si] ; FF /6
        pop word [si]        ; 8F /0
