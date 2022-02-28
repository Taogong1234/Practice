program name
    implicit none
    real(4)::dx,dt,mu,mc1,mc2
    real(4)::x,t
    real(4)::pi,a,b,c,d
    INTEGER::nx,nt,i,j
    real(4),ALLOCATABLE::u(:,:),phi(:,:),fx(:,:),px(:,:)
    pi= ACOS(-1.0)
    nx=101
    dx=2*pi/100
    mu=0.07
    dt=mu*dx
    nt=600
    ALLOCATE(u(nx,nt),phi(nx,nt),fx(nx,nt),px(nx,nt))

    
    fx=0
    ! 赋初始值与理论解
    do j=1,nt
        t=(j-1)*dt
        do i=1,nx
            x=(i-1)*dx
            phi(i,j)=exp((-(x-4*t)**2)/(4*mu*(t+1)))+exp((-(x-4*t-2*pi)**2)/(4*mu*(t+1)))
        end do
    end do


    do j=1,nt
        do i=1,nx
            if (i==1)then
                u(i,j)=-2*mu/phi(i,j)*((phi(i+1,j)-phi(nx-1,j))/2/dx)+4
            elseif(i==nx)then
                u(i,j)=-2*mu/phi(i,j)*((phi(2,j)-phi(i-1,j))/2/dx)+4
            else
                u(i,j)=-2*mu/phi(i,j)*((phi(i+1,j)-phi(i-1,j))/2/dx)+4
            end if
        end do
    end do




    fx=0
    fx(:,1)=u(:,1)
    ! !lax-wendroff格式
    ! do j = 1,nt-1
    !     do i = 1,nx
    !         if ( i/=1.and.i/=nx ) then
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(i-1,j))+(fx(i,j)*dt)**2/2/dx**2*(fx(i-1,j)+fx(i+1,j)-2*fx(i,j))
    !         elseif( i==1) then
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))+(fx(i,j)*dt)**2/2/dx**2*(fx(nx-1,j)+fx(i+1,j)-2*fx(i,j))
    !         else
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(2,j)-fx(i-1,j))+(fx(i,j)*dt)**2/2/dx**2*(fx(i-1,j)+fx(2,j)-2*fx(i,j))
    !         end if
    !     end do
    ! end do
    
    ! do j = 1,nt
    !     do i = 1,nx
    !             fx(i,j)=fx(i,j)+dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !     end do
    ! end do


    do j = 1,nt-1
        do i = 1,nx
            if ( i/=1.and.i/=nx ) then
                fx(i,j+1)=fx(i,j)-0.5*(fx(i+1,j)+fx(i-1,j))*dt/2/dx*(fx(i+1,j)-fx(i-1,j))&
                +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
            elseif(i==1) then
                fx(i,j+1)=fx(i,j)-0.5*(fx(i+1,j)+fx(nx-1,j))*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))&
                +dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
            else
                fx(i,j+1)=fx(i,j)-0.5*(fx(2,j)+fx(i-1,j))*dt/2/dx*(fx(2,j)-fx(i-1,j))&
                +dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
            end if
        end do
    end do



    ! do j = 1,nt-1
    !     do i = 1,nx
    !         if ( i/=1.and.i/=nx ) then
    !             fx(i,j+1)=fx(i,j)-0.5*(fx(i+1,j)+fx(i-1,j))*dt/dx*(fx(i,j)-fx(i-1,j))&
    !             +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         elseif(i==1) then
    !             fx(i,j+1)=fx(i,j)-0.5*(fx(i+1,j)+fx(nx-1,j))*dt/dx*(fx(i,j)-fx(nx-1,j))&
    !             +dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
    !         else
    !             fx(i,j+1)=fx(i,j)-0.5*(fx(2,j)+fx(i-1,j))*dt/dx*(fx(i,j)-fx(i-1,j))&
    !             +dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         end if
    !     end do
    ! end do



    ! do j = 1,nt-1
    !     do i = 1,nx
    !         if ( i/=1.and.i/=nx ) then !后插
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))&
    !             +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         elseif(i==1) then
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(nx-1,j))&
    !             +dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
    !         else
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))&
    !             +dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         end if
    !     end do
    ! end do

    ! do j = 1,nt-1
    !     do i = 1,nx
    !         if ( i/=1.and.i/=nx ) then !前插
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i+1,j)-fx(i,j))&
    !             +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         elseif(i==1) then
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i+1,j)-fx(i,j))&
    !             +dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
    !         else
    !             fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(2,j)-fx(i,j))&
    !             +dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
    !         end if
    !     end do
    ! end do








    OPEN(200,File='outfx1.dat',status='unknown')
    do j=1,nt
            write(200,25) fx(:,j)
    end do
    CLOSE(200)
25  format(101f11.3) 


    ! 输出数据-------------------------------------------------------------------------------
    OPEN(600,File='outgx.dat',status='unknown')
     do j=1,nt
             write(600,35) u(:,j)
     end do
     CLOSE(600)

35  format(101f11.3)
end program name