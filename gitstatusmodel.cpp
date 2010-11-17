#include "gitstatusmodel.h"
#include <QDebug>


GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    gitIndex(NULL)
{
    fileList << "test.cpp" << "a.out";
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    //return fileList.size();
    if(gitIndex==NULL)
        return 0;
    else
        return git_index_entrycount(gitIndex);
}


QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{
if(gitIndex==NULL)
    return(QVariant());
if (!index.isValid())
    return QVariant();
if (index.row() >= git_index_entrycount(gitIndex) || index.row() < 0)
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
