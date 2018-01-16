program project
implicit none 
real(kind=4) ::v,x,y
integer ::io_stat
character :: choice
character :: answer*1
do
        print*,'Enter X'
        read(*,*,iostat=io_stat)  x
        if(io_stat ==0) exit
        print*,"Sorry bad input , try again!"
        
end do 
do      
        print*,"Enter y"
        read(*,*,iostat=io_stat) y
        if(io_stat==0) exit
        print*,'Sorry bad input , enter y again!'
end do
print*,''
print*,''
print*,"Input 1 : ", x
print*,"Input 2  : ",y

print*,''
print*,''

do
print*,'== MENU OF CHOICES ==='

print*,''
print*,'Enter your choice'
print*,''
print*,'A --- Substract'
print*,'B----- Multiply'
print*,'C----- Power'
print*,'D----- Trigonmetry'

        print*,'Enter your choice'
        read(*,*)  choice


select case(choice)
	case('A','a')
		print*,'You chose Substract'
        	call substract(x,y)
        	print *, 'type y to continue or any other key to finish'
        	read *, answer
        	if (answer /= 'y') stop
	case('B','b')
                print*,'You chose Multiply'
                call multiply(x,y)
       	        print *, 'type y to continue or any other key to finish'
                read *, answer
        	if (answer /= 'y') stop
        case('C','c')
		print*,' You chose Power'
                call power(x,y)
                print *, 'type y to continue or any other key to finish'
                read *, answer
                if (answer /= 'y') stop
        case('D','d')
		print*,'You chose trigonometry'
                call trig(x,y)
                print *, 'type y to continue or any other key to finish'
                read *, answer
                if (answer /= 'y') stop
        case default
                 print*,'invalid choice'
                 
end select 
end do

end program project

subroutine substract(x,y)
implicit none
  real(kind=4):: x,y,z
  print*,'The difference of',x,' and ',y,' is ',x-y
end subroutine substract

subroutine multiply(x,y)
implicit none
  real(kind=4):: x,y
  print*,'The multiplication of',x,' and ',y,' is ',x*y
end subroutine multiply

subroutine power(x,y)
implicit none
  real(kind=4):: x,y
  print*,x,' power ',y,' is ',x**y
end subroutine power


subroutine trig(x,y)
implicit none
  real(kind=4):: x,y
  print*,' cosine of  ',x,' is ',cos(x)
  print*,' sine of  ',x,' is ',sin(x)
  print*,' cosine of  ',y,' is ',cos(y)
  print*,' sine of  ',y,' is ',cos(y)
end subroutine trig

