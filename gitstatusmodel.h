#ifndef GITSTATUSMODEL_H
#define GITSTATUSMODEL_H

#include <QAbstractListModel>

class GitStatusModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit GitStatusModel(QObject *parent = 0);

signals:

public slots:

};

#endif // GITSTATUSMODEL_H
