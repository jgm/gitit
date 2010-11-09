#ifndef COMMIT_H
#define COMMIT_H
class QProcess;
class QDateTime;
#include <QObject>

class Commit : public QObject
{
    Q_OBJECT
public:
    explicit Commit(QObject *parent = 0);
    QString author;
    QDateTime *dateTime;
    QStringList *files;



signals:

public slots:

};

#endif // COMMIT_H
