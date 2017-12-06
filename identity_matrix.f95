program matrix
implicit none 
integer , dimension(4,4) :: m
integer ::i,j

do i=1,4
  do j=1,4
      if(i==j) then 
         m(i,j)=1
      else
          m(i,j)=0
      end if
   end do
end do 

print*,m
print*,"====== another way"
 write(*,1) m
1     format(4i10)
end program
