#include "gitstatusmodel.h"
#include <QDebug>


GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent)
{
    fileList << "test.cpp" << "a.out";
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    //return fileList.size();
    return git_index_entrycount(gitIndex);
}


QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{

if (!index.isValid())
    return QVariant();
if (index.row() >= fileList.size() || index.row() < 0)
    return QVariant();
if (role == Qt::DisplayRole)
{
    return git_index_get(gitIndex, index.row())->path;
}
return QVariant();
}

void GitStatusModel::update(git_repository* gitRepo)
{
    qDebug() << "GitStatusModel::update(" << gitRepo << ")";
    gitIndex = git_repository_index(gitRepo);
    qDebug() << "gitIndex set";
}
