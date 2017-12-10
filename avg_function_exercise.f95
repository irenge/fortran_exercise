program func
!demonstrating use of user defined functions
implicit none
integer, parameter :: ikind=selected_real_kind(p=15)
real (kind=ikind) ,allocatable, dimension(:)::list
real (kind=ikind) :: average
integer :: n,i

print *, 'How many number'
read *, n
allocate(list(n))
do i=1,n
   print*,'Enter element',i,':'
   read*,list(i)
end do 
write(*,10) 'average = ',average(n,list)
10 format(a,f100.50)

deallocate(list)

end program func
!_____________________________________________

function average(n,list)

implicit none
integer, parameter :: ikind=selected_real_kind(p=15)

integer :: n,i
real (kind=ikind) ::total, average
real (kind=ikind), dimension(n)::list

total=0.0

do i=1,n
   total=total+list(i)
end do 
n=real(n)
average=(total/n)

end function average
