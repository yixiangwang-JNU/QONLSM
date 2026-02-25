      module value
      implicit double precision(a-h,o-z)
	real(8), parameter :: Pi=3.14159
      real(8), parameter :: xM=0.5             ! Dirac mass
cc 
      end module value
cc
**********************
*     main program
**********************
      program main
	use value
      implicit double precision (a-h,o-z)
      open(10,file='11.dat')
cc
      do 100 xk=-3,3,0.01
         temp=(xk*xk-1)**2+xM**2
         en1=sqrt(temp)
         en2=-en1         
         write(10,'(100f16.8)') xk,en1,en2 
100   continue      
cc
      end program main