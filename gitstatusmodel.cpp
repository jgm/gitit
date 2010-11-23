#include "gitstatusmodel.h"
#include <QDebug>
#include <QDateTime>


GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    gitIndex(NULL)
{
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    if(gitIndex==NULL)
        return 0;
    else
    {
        return git_index_entrycount(gitIndex);
    }
}

QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{
    if (gitIndex==NULL)
    {
        return QVariant();
    }
    if (!index.isValid())
        return QVariant();
    if (index.row() >= (int)git_index_entrycount(gitIndex) || index.row() < 0)
        return QVariant();
    if (role == Qt::DisplayRole)
    {
        return QString(git_index_get(gitIndex, index.row())->path);
    }
    return QVariant();
}
void GitStatusModel::update(git_repository* gitRepo)
{
    gitIndex = git_repository_index(gitRepo);
    git_index_read(gitIndex);
    //TODO check for off by 1
    //TODO check old size versus new size.
    emit dataChanged( createIndex(0,0), createIndex( git_index_entrycount(gitIndex),0 ) );
}
