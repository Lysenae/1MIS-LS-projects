// Projekt: PDS - L2 MitM
// Autor:   Daniel Klimaj; xklima22@stud.fit.vutbr.cz

#ifndef SOCKET_H
#define SOCKET_H

#include <cstdio>
#include <cerrno>

#include <sys/socket.h>
#include <unistd.h>

#include "types.h"

enum class SocketStatus
{
    NEW,
    OPENED,
    CLOSED
};

class Socket
{
public:
    Socket(int domain, int type, int protocol);
    int fd() const;
    SocketStatus status() const;
    SocketStatus open();
    SocketStatus close();
    int send_to( const void *buf, size_t len, int flags,
        const struct sockaddr *dest_addr, socklen_t addrlen);
    int recv_from(void *buf, size_t len, int flags,
        struct sockaddr *src_addr, socklen_t *addrlen);

private:
    int m_domain;
    int m_type;
    int m_protocol;
    int m_fd;
    SocketStatus m_status;
};

#endif // SOCKET_H
