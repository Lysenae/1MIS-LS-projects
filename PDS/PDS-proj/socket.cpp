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
    if(m_status != SocketStatus::OPENED)
    {
        m_fd     = socket(m_domain, m_type, m_protocol);
        if(m_fd == OP_FAIL)
            perror("Socket::open failed");
        else
            m_status = SocketStatus::OPENED;
    }
    return m_status;
}

SocketStatus Socket::close()
{
    int r = OP_FAIL;
    if(m_status == SocketStatus::OPENED)
    {
        r = ::close(m_fd);
        if(r == OP_FAIL)
            perror("Socket::close");
        else
            m_status = SocketStatus::CLOSED;
    }
    return m_status;
}
