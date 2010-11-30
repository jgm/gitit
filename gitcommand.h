#ifndef GITCOMMAND_H
#define GITCOMMAND_H


#include <QObject>
#include <QStringList>
#include <QString>
#include <QProcess>
class GitCommand : public QObject
{
    Q_OBJECT
public:
    explicit GitCommand(QObject *parent = 0);
    ~GitCommand();
    void status();

private:
    QStringList* fileList;
    QString repo;
    QProcess* gitProcess;
private slots:

    void statusOutput(int exitCode, QProcess::ExitStatus exitStatus);

signals:
    void status(QStringList files);

public slots:
    void setRepo(QString repo);

};

#endif // GITCOMMAND_H
