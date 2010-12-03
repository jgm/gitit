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
    QStringList run(QStringList arguments);
    void status();
    void lsIgnored();
    void add(QString filename);
    void log();
    QStringList branchList();
    QStringList remoteBranchList();

private:
    QStringList* defaultArgs;
    QString repo;
    QProcess* gitRunProcess;
    QProcess* gitStatusProcess;
    QProcess* gitLSIgnoredProcess;
    QProcess* gitLogProcess;
    QProcess* gitAddProcess;
    QProcess* gitBranchListProcess;

signals:
    void status(QStringList files);
    void lsIgnored(QStringList files);
    void log(QString log);

public slots:
    void setRepo(QString repo);

private slots:
    void statusOutput(int exitCode, QProcess::ExitStatus exitStatus);
    void lsIgnoredOutput(int exitCode, QProcess::ExitStatus exitStatus);
    void logOutput(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // GITCOMMAND_H
