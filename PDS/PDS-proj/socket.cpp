#include "socket.h"

Socket::Socket(int domain, int type, int protocol)
{
    m_domain   = domain;
    m_type     = type;
    m_protocol = protocol;
    m_fd       = OP_FAIL;
    m_status   = SocketStatus::NEW;
}

int Socket::fd() const
{
    return m_fd;
}

SocketStatus Socket::status() const
{
    return m_status;
}

SocketStatus Socket::open()
{
    m_fd     = socket(m_domain, m_type, m_protocol);
    if(m_fd == OP_FAIL)
        std::cerr << "Socket::open failed" << std::endl;
    else
        m_status = SocketStatus::OPENED;
    return m_status;
}

SocketStatus Socket::close()
{
    int r = -1;
    if(m_status == SocketStatus::OPENED)
    {
        r = shutdown(m_fd, 2);
        if(r == OP_FAIL)
            std::cerr << "Socket::close failed" << std::endl;
        else
            m_status = SocketStatus::CLOSED;
    }
    return m_status;
}
