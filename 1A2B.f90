Program Main
	Real :: x              ! for random number
    INTEGER :: ans(4)      ! answer of the game 
    Integer :: count_A = 0, count_B = 0   ! counting    
    Integer :: i,j,n         ! loop count
    Integer, Parameter :: round = 10 ! game round
    Integer :: guess(4)     ! player inputs(array)
    Character*5 :: input	! player inputs
	

	Call random_seed()           ! initialize seed
    ! initialize answer
    Call random_number(x)		 
    x = x*10.0
    ans(1) = x
    Call random_ans(1, ans)
    
    ! game instructions

	WRITE(*,*) "Here's a lock having a password composed by 4 differnet numbers"
    Write(*,*) "You have to find it out so that you can escape successfully"
  	WRITE(*,*) "You'll have 10 chances. Here are some hints:"
  	WRITE(*,*) "RIGHT number and RIGHT position: A"
    Write(*,*) "RIGHT number but WRONG position: B"
  	WRITE(*,*) "Good luck!"
	Write(*,*) "Enter q to quit..."
	Write(*,*) ''
	! main logic loop
    n = 1
    Do while (.TRUE.)
                  
		! verify inputs
		Call inputVerify(input)
        ! q means "quit"
        if (input == "q") then
          	exit
        Endif
		! string to int array
        Do i = 1,4
          	Read(input(i:i),"(I1)") guess(i)
        EndDo
		! comparing inputs with answer
        ! counting A and B
		Do i = 1, 4                                  
    		if (ans(i) == guess(i)) then
        		count_A = count_A + 1
        	else                                      
          		Do j = 1, 4
            		if (guess(i) == ans(j)) then
                		count_B = count_B + 1
               		Endif
            	EndDo
        	Endif
    	EndDo
        ! determine game state, win or not yet
        if (count_A /= 4) then
        	Write(*,*) count_A , "A" , count_B , "B"
            ! reset A, B
            count_A = 0
            count_B = 0
        else
          	Write(*,*) "Congratulations! You Win!"
            exit  ! jump out the loop
        endif
        
        if (n == round) then
          	Write(*,*) "Sorry, you lose!"
          	Write(*,*) "The answer is: ", ans(1), ans(2), ans(3), ans(4)
            Write(*,*) "Let's try again.." 
            Write(*,*) ""   
            ! reset
            n = 1
            Call random_number(x)		 
 		    x = x*10.0
 		    ans(1) = x
 		    Call random_ans(1, ans)
        else
          	Write(*,*) "You have ", round-n, " times to try..."
            n = n + 1
        Endif
    EndDo
    ! End of game logic loop
    Stop
! End of MAIN program
End Program Main

! 4 random number and no repeats
recursive subroutine random_ans(n, ans)
    integer, intent(inout) :: ans(4)
    integer, intent(inout) :: n
	Integer :: i, j, over
	j = n
	i = 1
	over = 0
    Do while (.TRUE.)
		Call random_number(x)
	    x = INT(x*10.0)
      	if (j == 4) then
        	exit
        Endif
		
		Do i = 1,j
	      	if (ans(i) == x) then
        		Call random_ans(j,ans)
                over = -1
            endif
        EndDo
    	
		if (over == -1) then
        	exit
        Endif
        
        ans(j+1) = x
        j = j + 1
    Enddo
End subroutine random_ans

! define a recursive subroutine
! this subroutine verifys the input and will keep recurse untill the input is correct or "q"
recursive subroutine inputVerify(input)
	! Declaration	
	Character*5, intent(inout) :: input
    Integer :: i
    Integer :: right, check
    ! End of Declaration
    
    	! check if anything needs to try again
    	check = 0 
    	Write(*,*) 'Please guess 4 number: '
		Read(*,*) input
        
		! enter more than 4 characters will be wrong
    	if (input(5:5) /= "") then
         	Write(*,*) "Please enter 4 numbers..."
            ! try again
	        Call inputVerify(input)
   		Endif
          
        ! enter q = "quit"    
       	Do while (input /= "q")
          	! check how many numbers are in the characters
     		right = 0
       		Do i = 1,4
				if (SCAN(input(i:i), "0123456789") /= 0) then
            		right = right + 1
            	Endif
      		EndDo
        	! not 4 numbers? need to try again
        	if (right /= 4) then
          			Write(*,*) "Please try again..."
                    check = -1  
            else
              	! numbers repeats? try again
              	Do i = 2,4
        			if (SCAN(input(i:4), input(i-1:i-1)) /= 0) then
            			Write(*,*) "Numbers repeat! Try again..."
                        check = -1
                        Exit
            		Endif
        		EndDo
       		Endif
            ! if anything goes wrong and needs to try again...
            if (check == -1) then
              	check = 0
              	Call inputVerify(input)
            else
              	exit
            Endif
        EndDo

End subroutine inputVerify
