program squareMatrix
implicit none
integer i,j,n
integer, allocatable, dimension(:,:)::m
print *, 'How big ?'
read *, n

allocate(m(n,n))
call diffMat(m,n)

 write(*,1) m
1 format(10i10)

deallocate(m)

end program 

subroutine diffMat(m,n)
implicit none 
integer i,j,n
integer :: m(n,n)

do i=1,n
  do j=1,n
      if(i==j) then
         m(i,j)=2
      else if(abs(i-j)==1) then
         m(i,j)=-1
      else
          m(i,j)=0
      end if
   end do
end do
end subroutine diffMat
