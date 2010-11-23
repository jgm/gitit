#include "gitstatusmodel.h"
#include <QDebug>
#include <QDateTime>


GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent),
    gitIndex(NULL)
{
    //fileList << "test.cpp" << "a.out";
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    if(gitIndex==NULL)
        return 0;
    else
    {
        git_index_read(gitIndex);
        qDebug() << "git_index_endtrycount(gitIndex)" << git_index_entrycount(gitIndex);
        return git_index_entrycount(gitIndex);
    }
}

QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{
    qDebug() << QDateTime::currentDateTime() << "GitStatModel::data(" << index << "," << role << ") called";
    if (gitIndex==NULL)
    {
        qDebug() << "gitIndex is not set.";
        return QVariant();
    }
    if (!index.isValid())
        return QVariant();
    if (index.row() >= (int)git_index_entrycount(gitIndex) || index.row() < 0)
        return QVariant();
    //if (role == Qt::DisplayRole)
    //{
    qDebug() << "index.row()" << index.row() << git_index_get(gitIndex, index.row())->path;
        return QString(git_index_get(gitIndex, index.row())->path);
    //}
    return QVariant();
}
void GitStatusModel::update(git_repository* gitRepo)
{
    qDebug() << "GitStatusModel::update(" << gitRepo << ")";
    gitIndex = git_repository_index(gitRepo);
    //TODO check for off by 1
    //TODO check old size versus new size.
    emit dataChanged( createIndex(0,0), createIndex( git_index_entrycount(gitIndex),0 ) );
    qDebug() << "gitIndex set";
}
