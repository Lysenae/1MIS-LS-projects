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

private:
    int m_domain;
    int m_type;
    int m_protocol;
    int m_fd;
    SocketStatus m_status;
};

#endif // SOCKET_H
