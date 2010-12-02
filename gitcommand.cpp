#include "gitcommand.h"
#include <QStringList>
#include <QSettings>
#include <QProcess>

GitCommand::GitCommand(QObject *parent) :
    QObject(parent),
    defaultArgs(new QStringList),
    repo(),
    gitRunProcess(new QProcess),
    gitStatusProcess(new QProcess),
    gitLSIgnoredProcess(new QProcess),
    gitAddProcess(new QProcess)
{
    connect(gitStatusProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(statusOutput(int, QProcess::ExitStatus)));
    connect(gitLSIgnoredProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(lsIgnoredOutput(int, QProcess::ExitStatus)));
}
GitCommand::~GitCommand()
{
    delete defaultArgs;
    delete gitRunProcess;
    delete gitStatusProcess;
    delete gitLSIgnoredProcess;
    delete gitAddProcess;
}
void GitCommand::run(QStringList arguments)
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args = *defaultArgs;
    args << arguments;
    gitRunProcess->start(proc, args);
    gitRunProcess->waitForFinished(3000);
}
void GitCommand::status()
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args = *defaultArgs;
    args << "status" << "--porcelain";
    gitStatusProcess->start(proc, args);

}
void GitCommand::statusOutput(int exitCode, QProcess::ExitStatus exitStatus)
{
    QByteArray result = gitStatusProcess->readAll();
    QString resultString(result);

    QStringList fileList = resultString.split('\n',QString::SkipEmptyParts);
    emit status(fileList);

}
void GitCommand::lsIgnored()
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args = *defaultArgs;
    args << "ls-files" << "--others" << "--ignored" << "--exclude-standard";
    gitLSIgnoredProcess->start(proc, args);
}
void GitCommand::lsIgnoredOutput(int exitCode, QProcess::ExitStatus exitStatus)
{
    QByteArray result = gitLSIgnoredProcess->readAll();
    QString resultString(result);

    QStringList fileList = resultString.split('\n',QString::SkipEmptyParts);
    emit lsIgnored(fileList);
}
void GitCommand::add(QString filename)
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args = *defaultArgs;
    args << "add" << filename;
    gitAddProcess->start(proc, args);
    gitAddProcess->waitForFinished(5000);
}

void GitCommand::setRepo(QString repo)
{
    *defaultArgs = QStringList() << "--git-dir" << repo + "/.git" << "--work-tree" << repo;
}
