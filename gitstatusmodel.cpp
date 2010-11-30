#include "gitstatusmodel.h"
#include <QDebug>
#include <QDateTime>
#include <QSettings>

GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    process(new QProcess(this)),
    fileList(new QStringList)
{
    connect(process, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(readOutput(int, QProcess::ExitStatus)) );
}
GitStatusModel::~GitStatusModel()
{
    delete process;
    delete fileList;
}
int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    return fileList->count();
}
QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();
    if (index.row() >= fileList->count() || index.row() < 0)
        return QVariant();
    if (role == Qt::DisplayRole)
    {
        return (*fileList)[index.row()];
    }
    return QVariant();
}
void GitStatusModel::update(QString repo)
{
    QSettings settings;
    QString proc = settings.value("gitPath").toString();
    QStringList args;
    args << "--git-dir" << repo + "/.git" << "--work-tree" << repo << "status" << "--porcelain";
    process->start(proc, args);
}
void GitStatusModel::readOutput(int exitCode, QProcess::ExitStatus exitStatus)
{

}
