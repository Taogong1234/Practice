program name

    implicit none
    integer:: i,j,iter
    integer,PARAMETER::nx=600,ny=600,steps=10000
    REAL(8):: START1,FINISH1,time
    real::x(2)=(/0,2/),y(2)=(/0,2/)
    real(4)::dx,dy
    real(8)::factor
    real(4),ALLOCATABLE::p(:,:),pn(:,:),b(:,:)
    dx=max(x(1),x(2))/(nx-1)
    dy=max(y(1),y(2))/(ny-1)
    ALLOCATE(p(nx,ny),pn(nx,ny),b(nx,ny))

    CALL CPU_TIME(START1)
    !初始化
    p=0
    pn=0
    p(1,:)=0
    p(nx,:)=0
    p(:,0)=0
    p(:,ny)=0
    b=0
    b(int(0.25*(nx-1)),int(0.25*(ny-1)))=100
    b(int(0.75*(nx-1)),int(0.75*(ny-1)))=-100
    factor=1
    
   
    do iter=1,steps
        do j=2,ny-1
            do i=2,nx-1
                pn(i,j)=((p(i+1,j)+p(i-1,j))*dy**2+(p(i,j+1)+p(i,j-1))*dx**2-b(i,j)*dx**2*dy**2)/2/(dx**2+dy**2)
            end do
        end do
        p=pn
        ! print*,'迭代步数:',iter
    end do

    OPEN(100,file='poisson.dat',status="unknown")
    do i=1,nx
    write(100,25) (p(i,:))
    end do
25  FORMAT(*(f9.5))

    close(100)
    CALL CPU_TIME(FINISH1)
    time=FINISH1-START1
    print *,'CPU_TIME运算时间为(s):',time

end program name