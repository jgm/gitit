#include "gitcommand.h"
#include <QStringList>
#include <QSettings>
#include <QProcess>

GitCommand::GitCommand(QObject *parent) :
    QObject(parent),
    fileList(new QStringList),
    repo(),
    gitProcess(new QProcess)
{
}
GitCommand::~GitCommand()
{
    delete fileList;
}
void GitCommand::status()
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args;
    args << "--git-dir" << repo + "/.git" << "--work-tree" << repo << "status" << "--porcelain";

    connect(gitProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(statusOutput(int, QProcess::ExitStatus)));
    gitProcess->start(proc, args);

}
void GitCommand::statusOutput(int exitCode, QProcess::ExitStatus exitStatus)
{
    QByteArray result = gitProcess->readAll();
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
}
