import React from 'react'
import { defaultProps } from 'recompose'


export class UserTable extends React.Component {
    render () {
        const L = this.props.labels
        const { list, className } = this.props
        return (<table className={className}>
            <thead>
                <tr className="d-none d-sm-table-row">
                    <th>{L.fullName}</th>
                    <th>{L.ident}</th>
                    <th>{L.email}</th>
                    <th className="controls"/>
                </tr>
            </thead>
            <tbody>
                {list.map((item, idx) => {
                    const name = <React.Fragment>
                        {item.lastName? ` ${item.lastName}` : ''}
                        {item.firstName? ` ${item.firstName}` : ''}
                        {item.patronymic? ` ${item.patronymic}` : ''}
                    </React.Fragment>
                    return (<React.Fragment key={idx}>
                        <tr className="d-none d-sm-table-row">
                            <td>{name}</td>
                            <td>{item.ident}</td>
                            <td>{item.email}</td>
                            <td>
                                <a
                                    href={item.updateUrl}
                                    title={L.update}>
                                    <i className="fas fa-edit"/>
                                </a>
                            </td>
                        </tr>

                        <tr className="d-sm-none">
                            <td>
                                <div className="card">
                                    <div className="card-body">
                                        <h5 className="card-title">{name}</h5>
                                        <h6 className="card-subtitle mb-2 text-muted">{item.ident}</h6>
                                        {item.ident != item.email && <h6 className="card-subtitle mb-2 text-muted">{item.email}</h6>}

                                        <a
                                                href={item.updateUrl}
                                                title={L.title}
                                                className="card-link">
                                            <i className="fas fa-edit"/>
                                            {L.update}
                                        </a>
                                    </div>
                                </div>
                            </td>
                        </tr>
                    </React.Fragment>)
                })}
            </tbody>
        </table>)
    }
}




const withDeafaultProps = defaultProps({
    labels: {
        fullName: 'ФИО',
        ident: 'Логин',
        email: 'Эл.почта',
        create: 'Создать',
        update: 'Править'
    },
    className: "table table-hover",
    list: []
})

export default withDeafaultProps(UserTable)
