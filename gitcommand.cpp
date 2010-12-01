#include "gitcommand.h"
#include <QStringList>
#include <QSettings>
#include <QProcess>

GitCommand::GitCommand(QObject *parent) :
    QObject(parent),
    defaultArgs(new QStringList),
    repo(),
    gitStatusProcess(new QProcess),
    gitLSProcess(new QProcess)
{
    connect(gitStatusProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(statusOutput(int, QProcess::ExitStatus)));

}
GitCommand::~GitCommand()
{
    delete defaultArgs;
    delete gitStatusProcess;
    delete gitLSProcess;
}
void GitCommand::status()
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args = *defaultArgs << "status" << "--porcelain";
    gitStatusProcess->start(proc, args);

}
void GitCommand::statusOutput(int exitCode, QProcess::ExitStatus exitStatus)
{
    QByteArray result = gitStatusProcess->readAll();
    QString resultString(result);

    QStringList fileList = resultString.split('\n',QString::SkipEmptyParts);
    emit status(fileList);

}
void GitCommand::setRepo(QString repo)
{
    *defaultArgs = QStringList() << "--git-dir" << repo + "/.git" << "--work-tree" << repo;
    status();
}
