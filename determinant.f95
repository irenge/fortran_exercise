program readMat
implicit none
! declare 
real,dimension(3,3) :: m,cofm
real :: minor(2,2),cofactor,det
integer :: i,j
! open a saved 3x3 matrix  file 
open(10,file='threeByThree.txt')
! read in 
read(10,*) m
! displaying format
10 format(3f6.2)
!display matrix 
write(*,10)  m
print*,'====m(1,1),m(2,1),m(3,1)'
print*,m(1,1),m(2,1),m(3,1)
print*,"Calculate determinant "

det=0.0
do i=1,3
        det = det + ((-1)**(i+1))*m(i,1)*cofactor(i,1,m)
end do 

print*,'The determinant is ',det 

print*,"============COFACTOR ============"

!print*,' Cofactor of ',m(1,2),' is '
! calculate cofactor here 
do i=1,3
    do j=1,3
        cofm(i,j)=((-1)**(i+j))*cofactor(i,j,m)
    end do
end do

11 format(3f6.2)
!print cofactor 
write(*,11) cofm








end program 
!===================================
function cofactor(i,j,mat)
implicit none
!declare variable
real :: mat(3,3),minor(2,2)= reshape((/1,2,3,4/), (/2,2/))
integer :: elrow,elcol,i,j,k,l
real,dimension(1:4)::d
real::cofactor

! cof – the cofactor of matrix mat for element i,j

!print*," ===== print minor ==== "
! calculate minor
k=1
do elrow=1,3
   do elcol =1,3
   if (elrow /=i) then 
      if(elcol /=j) then 
     d(k)=mat(elrow,elcol)
     k=k+1     
   end if 
   end if
   end do
end do

minor= transpose(reshape((/d(:)/), (/2,2/)))
!write(*,8) minor

!8 format(2f6.2)

cofactor = minor(1,1)*minor(2,2)-minor(2,1)*minor(1,2)

!print*,'Cofactor=',cof

end function cofactor
