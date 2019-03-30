import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'
import 'whatwg-fetch'

import UserForm from './form'
import { setUserData } from './utils';

export class CreateUser extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            user: {}
        }
    }
    updateFormData (data) {
        this.setState(s => {
            let d = _.merge({}, s, { user: data })
            // do not deep merge user rights array
            d.rights = s.user.rights
            return d
        })
    }
    createUser (data) {
        let formData = new FormData()
        const user = data
        setUserData(formData, user)
        fetch(this.props.apiUrl, {
            method: 'POST',
            body: formData
        }).then(response => {
            console.log(response)
            response.json()
        }).then(json => console.log)
    }
    render () {
        return (<UserForm
                onChange={data => this.updateFormData(data)}
                onSubmit={data => this.createUser(data)}
                { ... this.props.user }
                accessRights={this.props.accessRights}
                />)
    }
}

const withDefaultProps = defaultProps({
    labels: {
        nameTitle: "ФИО",
        identTitle: "Логин",
        emailTitle: "Эл.почта",
        passwordTitle: "Пароль",
        save: "Создать"
    },
    id: _.uniqueId(),
    accessRights: []
})

export default withDefaultProps(CreateUser)