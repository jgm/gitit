#include "gitstatusmodel.h"

GitStatusModel::GitStatusModel(QObject *parent) :
    QAbstractListModel(parent)
{
    fileList << "test.cpp" << "a.out";
}

int GitStatusModel::rowCount(const QModelIndex & /* parent */) const
{
    return fileList.size();
}


QVariant GitStatusModel::data(const QModelIndex &index, int role) const
{

if (!index.isValid())
    return QVariant();
if (role == Qt::DisplayRole)
    return fileList.at(index.row());
return QVariant();
}
