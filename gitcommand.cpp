#include "gitcommand.h"
#include <QStringList>
#include <QSettings>
#include <QProcess>

GitCommand::GitCommand(QObject *parent) :
    QObject(parent),
    fileList(new QStringList),
    repo(),
    gitStatusProcess(new QProcess)
{
    connect(gitStatusProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(statusOutput(int, QProcess::ExitStatus)));

}
GitCommand::~GitCommand()
{
    delete fileList;
    delete gitStatusProcess;
}
void GitCommand::status()
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args;
    args << "--git-dir" << repo + "/.git" << "--work-tree" << repo << "status" << "--porcelain";

    gitStatusProcess->start(proc, args);

}
void GitCommand::statusOutput(int exitCode, QProcess::ExitStatus exitStatus)
{
    QByteArray result = gitStatusProcess->readAll();
    QString resultString(result);

    QStringList fileList = resultString.split('\n',QString::SkipEmptyParts);
    /*QRegExp rx("^(.\\S).*$"); // " M filname"  "MM filename" "AM filename"
    rx.setPatternSyntax(QRegExp::RegExp2);
    fileList = fileList.filter(rx);*/

    emit status(fileList);

}
void GitCommand::setRepo(QString repo)
{
    this->repo=repo;
    status();
}
