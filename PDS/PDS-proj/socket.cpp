// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#include "socket.h"

Socket::Socket(int domain, int type, int protocol)
{
    m_domain   = domain;
    m_type     = type;
    m_protocol = protocol;
    m_fd       = OP_FAIL;
    m_status   = SocketStatus::New;
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
    if(m_status != SocketStatus::Opened)
    {
        m_fd     = socket(m_domain, m_type, m_protocol);
        if(m_fd == OP_FAIL)
            perror("Socket::open failed");
        else
            m_status = SocketStatus::Opened;
    }
    return m_status;
}

SocketStatus Socket::close()
{
    int r = OP_FAIL;
    if(m_status == SocketStatus::Opened)
    {
        r = ::close(m_fd);
        if(r == OP_FAIL)
            perror("Socket::close");
        else
            m_status = SocketStatus::Closed;
    }
    return m_status;
}

int Socket::send_to(const void *buf, size_t len, int flags,
const sockaddr *dest_addr, socklen_t addrlen)
{
    if(m_status == SocketStatus::Opened)
        return sendto(m_fd, buf, len, flags, dest_addr, addrlen);
    return -1;
}

int Socket::recv_from(void *buf, size_t len, int flags, sockaddr *src_addr, socklen_t *addrlen)
{
    if(m_status == SocketStatus::Opened)
        return recvfrom(m_fd, buf, len, flags, src_addr, addrlen);
    return -1;
}
