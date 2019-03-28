import React from 'react'
import { defaultProps } from 'recompose'


export class UserTable extends React.Component {
    render () {
        const L = this.props.labels
        const { list, className } = this.props
        return (<table className={className}>
            <thead>
                <tr>
                    <th>{L.ident}</th>
                    <th>{L.email}</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                {list.map((item, idx) => {
                    return (<tr key={idx}>
                        <td>{item.ident}</td>
                        <td>{item.email}</td>
                        <td></td>
                    </tr>)
                })}
            </tbody>
        </table>)
    }
}




const withDeafaultProps = defaultProps({
    labels: {
        ident: 'Логин',
        email: 'Эл.почта',
        create: 'Создать',
        updaet: 'Править'
    },
    className: "table table-hover",
    list: []
})

export default withDeafaultProps(UserTable)
