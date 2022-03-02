program name
    use mpi
    implicit none
    real(4)::dx,dt,mu,mc1,mc2
    real(4)::x,t
    real(4)::pi,a,b,c,d
    INTEGER::nx,nt,i,j
    real(4),ALLOCATABLE::u(:,:),phi(:,:),fx(:,:),px(:,:)
    INTEGER::ierr,myid,num
    pi= ACOS(-1.0)  !自定义pi
    nx=101
    dx=2*pi/100
    mu=0.07
    dt=mu*dx
    nt=181
    ALLOCATE(u(nx,nt),phi(nx,nt),fx(nx,nt),px(nx,nt))
    
    
    call mpi_init(ierr)
    call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num,ierr)
    print*,'进程号为:',myid,'总进程数为:',num

    
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


    if (myid==0) then
        fx=0
        fx(:,1)=u(:,1)
        ! lax格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    fx(i,j+1)=0.5*(fx(i+1,j)+fx(i-1,j))-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(i-1,j))&
                    +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*0.5*(fx(i+1,j)+fx(i-1,j)))/dx**2
                elseif(i==1) then
                    fx(i,j+1)=0.5*(fx(i+1,j)+fx(nx-1,j))-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))&
                    +dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*0.5*(fx(i+1,j)+fx(nx-1,j)))/dx**2
                else
                    fx(i,j+1)=0.5*(fx(2,j)+fx(i-1,j))-fx(i,j)*dt/2/dx*(fx(2,j)-fx(i-1,j))&
                    +dt*mu*(fx(2,j)+fx(i-1,j)-2*0.5*(fx(2,j)+fx(i-1,j)))/dx**2
                end if
            end do
        end do
    

        OPEN(100,File='outfx1.dat',status='unknown')
        do j=1,nt
                write(100,25) fx(:,j)
        end do
        CLOSE(100)
    endif

25  format(101f7.3) 

    if (myid==1) then
        fx=0
        fx(:,1)=u(:,1)
        ! !lax-wendroff格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(i-1,j))+(fx(i,j)*dt)**2/2/dx**2*(fx(i-1,j)+fx(i+1,j)-2*fx(i,j))&
                    +dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                elseif( i==1) then
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))&
                    +(fx(i,j)*dt)**2/2/dx**2*(fx(nx-1,j)+fx(i+1,j)-2*fx(i,j))+dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
                else
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/2/dx*(fx(2,j)-fx(i-1,j))+(fx(i,j)*dt)**2/2/dx**2*(fx(i-1,j)+fx(2,j)-2*fx(i,j))&
                    +dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                end if
            end do
        end do
        
        OPEN(200,File='outfx2.dat',status='unknown')
        do j=1,nt
                write(200,25) fx(:,j)
        end do
        CLOSE(200)
    endif


    if(myid==2) then
        fx=0
        fx(:,1)=u(:,1)
        ! Maccormack格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    mc1=fx(i,j)-fx(i,j)*dt/dx*(fx(i+1,j)-fx(i,j))
                    mc2=fx(i-1,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-fx(i,j)*dt/2/dx*(mc1-mc2)+dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                elseif( i==1) then
                    mc1=fx(i,j)-fx(i,j)*dt/dx*(fx(i+1,j)-fx(i,j))
                    mc2=fx(nx-1,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(nx-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-fx(i,j)*dt/2/dx*(mc1-mc2)+dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
                else
                    mc1=fx(i,j)-fx(i,j)*dt/dx*(fx(2,j)-fx(i,j))
                    mc2=fx(i-1,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-fx(i,j)*dt/2/dx*(mc1-mc2)+dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                end if
            end do
        end do

        OPEN(300,File='outfx3.dat',status='unknown')
        do j=1,nt
                write(300,25) fx(:,j)
        end do
        CLOSE(300)
    endif

    if (myid==3) then
        fx=0
        fx(:,1)=u(:,1)
        ! ! Beam-Warming格式
        do j = 1,nt-1
            do i = 1,nx
                if  ( i==1 ) then
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(nx-1,j))-fx(i,j)*dt/2/dx*(1-fx(i,j)*dt/dx)&
                    *(fx(i,j)+fx(nx-2,j)-2*fx(nx-1,j))+dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
                elseif( i==2) then
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))-fx(i,j)*dt/2/dx*(1-fx(i,j)*dt/dx)&
                    *(fx(i,j)+fx(nx-1,j)-2*fx(i-1,j))+dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                else
                    fx(i,j+1)=fx(i,j)-fx(i,j)*dt/dx*(fx(i,j)-fx(i-1,j))-fx(i,j)*dt/2/dx*(1-fx(i,j)*dt/dx)&
                    *(fx(i,j)+fx(i-2,j)-2*fx(i-1,j))+dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                end if
            end do
        end do

        OPEN(400,File='outfx4.dat',status='unknown')
        do j=1,nt
                write(400,25) fx(:,j)
        end do
        CLOSE(400)
    endif

    if(myid==4)then
        fx=0
        fx(:,1)=u(:,1)
        ! cip格式    ! 给px赋初始值
        do i=1,nx
            if (i==1) then 
                px(i,1)=(fx(i+1,1)-fx(nx-1,1))/2/dx
            elseif(i==nx) then
                px(i,1)=(fx(2,1)-fx(i-1,1))/2/dx
            else 
                px(i,1)=(fx(i+1,1)-fx(i-1,1))/2/dx
            end if
        end do

        do j = 1,nt-1
            do i = 1,nx
                if  ( i==1 ) then
                a=(px(i,j)+px(nx-1,j))/dx**2-(2*fx(i,j)-2*fx(nx-1,j))/dx**3
                b=(2*px(i,j)+px(nx-1,j))/dx-(3*fx(i,j)-3*fx(nx-1,j))/dx**2
                c=px(i,j)
                d=fx(i,j)
                fx(i,j+1)=a*(-fx(i,j)*dt)**3+b*(-fx(i,j)*dt)**2+c*(-fx(i,j)*dt)+d+dt*mu*(fx(i+1,j)+fx(nx-1,j)-2*fx(i,j))/dx**2
                px(i,j+1)=3*a*(-fx(i,j)*dt)**2+2*b*(-fx(i,j)*dt)+c

                elseif(i==nx) then
                    a=(px(i,j)+px(i-1,j))/dx**2-(2*fx(i,j)-2*fx(i-1,j))/dx**3
                    b=(2*px(i,j)+px(i-1,j))/dx-(3*fx(i,j)-3*fx(i-1,j))/dx**2
                    c=px(i,j)
                    d=fx(i,j)
                    fx(i,j+1)=a*(-fx(i,j)*dt)**3+b*(-fx(i,j)*dt)**2+c*(-fx(i,j)*dt)+d+dt*mu*(fx(2,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                    px(i,j+1)=3*a*(-fx(i,j)*dt)**2+2*b*(-fx(i,j)*dt)+c
                else
                    a=(px(i,j)+px(i-1,j))/dx**2-(2*fx(i,j)-2*fx(i-1,j))/dx**3
                    b=(2*px(i,j)+px(i-1,j))/dx-(3*fx(i,j)-3*fx(i-1,j))/dx**2
                    c=px(i,j)
                    d=fx(i,j)
                    fx(i,j+1)=a*(-fx(i,j)*dt)**3+b*(-fx(i,j)*dt)**2+c*(-fx(i,j)*dt)+d+dt*mu*(fx(i+1,j)+fx(i-1,j)-2*fx(i,j))/dx**2
                    px(i,j+1)=3*a*(-fx(i,j)*dt)**2+2*b*(-fx(i,j)*dt)+c
                end if
            end do
        end do

    
        ! 输出数据-------------------------------------------------------------------------------
        OPEN(500,File='outfx5.dat',status='unknown')
        OPEN(600,File='outgx.dat',status='unknown')
        do j=1,nt
                write(500,25) fx(:,j)
                write(600,35) u(:,j)
        end do
        CLOSE(500)
        CLOSE(600)
    endif
    35  format(101f7.3)


    if(myid==5)then
        fx=0
        fx(:,1)=u(:,1)

        ! 拼凑格式
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
        OPEN(700,File='outfx6.dat',status='unknown')
        do j=1,nt
                write(700,25) fx(:,j)
        end do
        CLOSE(700)
    endif

    call MPI_Finalize(ierr)
    
end program name