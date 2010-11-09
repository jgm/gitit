#ifndef CURRENTUSERINFO_H
#define CURRENTUSERINFO_H
class QProcess;
#include <QObject>

class CurrentUserInfo : public QObject
{
    Q_OBJECT
public:
    explicit CurrentUserInfo(QObject *parent = 0);

signals:

public slots:

};

#endif // CURRENTUSERINFO_H
