#include "gitstatusmodel.h"
#include <QDebug>


GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    gitIndex(NULL)
{
    //fileList << "test.cpp" << "a.out";
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    //return fileList.size();
    if(gitIndex==NULL)
        return 1;
    else
    {
        git_index_read(gitIndex);
        return git_index_entrycount(gitIndex);
    }
}

QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{
    qDebug() << "GitStatModel::data() called";
    if (gitIndex==NULL)
        return QVariant();
    if (!index.isValid())
        return QVariant();
    if (index.row() >= (int)git_index_entrycount(gitIndex) || index.row() < 0)
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
