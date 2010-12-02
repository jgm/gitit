#ifndef GITCOMMAND_H
#define GITCOMMAND_H


#include <QObject>
#include <QStringList>
#include <QString>
#include <QProcess>
/**
* This class is responsible for all interactions with the git command, and therefor the git databases.
*/
class GitCommand : public QObject
{
    Q_OBJECT
public:
    explicit GitCommand(QObject *parent = 0);
    ~GitCommand();
    void status();
    void lsIgnored();

private:
    QStringList* defaultArgs;
    QString repo;
    QProcess* gitStatusProcess;
    QProcess* gitLSIgnoredProcess;

signals:
    void status(QStringList files);
    void lsIgnored(QStringList files);

public slots:
    void setRepo(QString repo);

private slots:
    void statusOutput(int exitCode, QProcess::ExitStatus exitStatus);
    void lsIgnoredOutput(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // GITCOMMAND_H
