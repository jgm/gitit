#ifndef HISTORY_H
#define HISTORY_H
class QProcess;
#include <QObject>

class History : public QObject
{
    Q_OBJECT
public:
    explicit History(QObject *parent = 0);

signals:

public slots:

};

#endif // HISTORY_H
