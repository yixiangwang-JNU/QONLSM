********************************************************************
*     Calculate the chemical potential in a nodal-ring semimetal 
********************************************************************
      module value
      implicit double precision (a-h,o-z)      
      real(8), parameter :: Pi=3.14159 
      real(8), parameter :: xM=0.5            ! Dirac mass 
cc    real(8), parameter :: xmu=0.8           ! chemical potential 
      real(8), parameter :: density0=3.962    ! the chosen value 1.672,3.108,6.755  
      integer, parameter :: Ncut=1000         ! cutoff
cc      
      end module value
cc      
**********************
*     main program
**********************
      program main
      use value
      implicit double precision (a-h,o-z)
      character(100) filename1,filename2 
      write(filename1,'(a100)') 'LLs.dat'
      write(filename2,'(a100)') 'mu=0.95,FFT.dat'
      open(10,file=filename1)
      open(20,file=filename2)
      open(30,file='30.dat')
cc
      do 101 xBinv=0.2,100.001,0.001
cc
      xB=1./xBinv    
      write(*,*) xBinv 
cc    
      densityb=0 
      do 100 xmu=xM+0.001,20.1,0.001
      densitya=densityb
      densityb=0 
      do 200 i=0,Ncut
         density1=0            ! setting the initial values 
         density2=0     
cc        
         Xn=2*i*xB+xB-1 
         aa=sqrt(xmu**2-xM**2)-Xn 
         if(aa>0) density1=sqrt(aa)*5.0661*xB 
cc       
         bb=-sqrt(xmu**2-xM**2)-Xn 
         if(Xn<0 .and. bb>0) density2=sqrt(bb)*5.0661*xB 
         
         densityb=densityb+density1-density2
         if(densityb>density0) exit          
200   continue
cc
      if(densityb>density0) then 
         error1=density0-densitya
         error2=densityb-density0
         if(error1>error2) then
            xmu1=xmu
            write(20,'(2f16.8,I4)') xBinv,xmu1
         else 
            xmu1=xmu-0.001
            write(20,'(100f16.8)') xBinv,xmu1               
         end if
cc          
         exit 
      end if 
100   continue 
cc
101   continue
cc
      end program main
cc