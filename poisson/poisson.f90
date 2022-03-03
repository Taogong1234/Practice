program name

    implicit none
    integer:: i,j,iter
    integer,PARAMETER::nx=51,ny=51
    real::x(2)=(/0,2/),y(2)=(/0,1/)
    real(4)::dx,dy
    real(8)::factor
    real(4),ALLOCATABLE::p(:,:),pn(:,:),b(:,:)
    dx=max(x(1),x(2))/(nx-1)
    dy=max(y(1),y(2))/(ny-1)
    ALLOCATE(p(nx,ny),pn(nx,ny),b(nx,ny))

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
    
   
    do iter=1,100
        do j=2,ny-1
            do i=2,nx-1
                pn(i,j)=((p(i+1,j)+p(i-1,j))*dy**2+(p(i,j+1)+p(i,j-1))*dx**2-b(i,j)*dx**2*dy**2)/2/(dx**2+dy**2)
            end do
        end do
        p=pn
        print*,'迭代步数:',iter
    end do

    OPEN(100,file='poisson.dat',status="unknown")
    do j=1,nx
    write(100,25) (p(:,j))
    end do
25  FORMAT(*(f9.5))

    close(100)

end program name