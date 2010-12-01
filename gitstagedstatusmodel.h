#ifndef GITSTAGEDSTATUSMODEL_H
#define GITSTAGEDSTATUSMODEL_H

#include <QAbstractListModel>
#include <QStringList>

class GitStagedStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitStagedStatusModel(QObject *parent = 0);
    ~GitStagedStatusModel();
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
signals:

public slots:
    void update(QStringList files);

private:
        //void updateFileList();
        QStringList *fileList;
};

#endif // GITSTAGEDSTATUSMODEL_H
