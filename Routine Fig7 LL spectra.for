*****************************************************
*     Calculate the LLs of a nodal-ring semimetal 
*****************************************************
      module value
      implicit double precision (a-h,o-z)      
      real(8), parameter :: Pi=3.14159 
      real(8), parameter :: xM=0.5              ! Dirac mass
      integer, parameter :: Ncut=20
      real(8) en(Ncut+1)
cc      
      end module value
cc      
**********************
*     main program
**********************
      program main
      use value
      implicit double precision (a-h,o-z)
      character(100) filename 
      real(8) xBinv(10)
      xBinv(1)=0.2
      xBinv(2)=0.5
      xBinv(3)=1
      xBinv(4)=2
      xBinv(5)=3
      xBinv(6)=5
      xBinv(7)=8
      xBinv(8)=10
      xBinv(9)=12
      xBinv(10)=15
cc
      do 101 i=1,10
      xB=1./xBinv(i)    
      write(filename,'(a5,f4.1,a4)') 'Binv=',xBinv(i),'.dat'
      open(10,file=filename)        
cc
      do 100 zk=-2,2,0.01
      do 200 n=0,Ncut    
         aa=2*n*xB+xB-1+zk*zk
         en(n+1)=sqrt(aa*aa+xM**2)          
200   continue      
cc 
      write(10,'(1000f16.8)') zk,(en(n+1),n=0,Ncut)
100   continue          
cc
101   continue       
cc      
      end program main
cc