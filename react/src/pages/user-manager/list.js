import React from 'react'
import { defaultProps } from 'recompose'

import UserTable from './user-table'

import _ from 'lodash'
import 'whatwg-fetch'


export class ListUser extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            list: []
        }
    }
    fetchList () {
        const self = this
        fetch(this.props.apiUrl)
            .then(res => res.json())
            .then(j => {
                let users = j.users
                console.log(j, users)
                self.setState(s => _.merge({}, s, { list: users }))
            })
    }
    componentDidMount () {
        let list = []
        this.fetchList()
        this.state.list = list
    }
    render () {
        const L = this.props.labels
        return (<React.Fragment>
            <h1>{L.title}</h1>
            <UserTable list={this.state.list}/>
        </React.Fragment>)
    }
}

const withDeafaultProps = defaultProps({
    labels: {
        title: 'Пользователи'
    }
})

export default withDeafaultProps(ListUser)