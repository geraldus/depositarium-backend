import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'
import 'whatwg-fetch'

import UserForm from './form'
import { setUserData } from './utils'
import '../../../utils'


export class UpdateUser extends React.Component {
    constructor (props) {
        super(props)
        console.log('UpdateUser.props', props)
        this.state = {
            user: this.props.user
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
    updateUser (data) {
        console.log(data)
        const user = data
        let formData = new FormData()
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
        const { user } = this.state
        let ident = user.ident
        if (user.firstName != '' || user.lastName != '') {
            ident = [
                user.firstName || '',
                user.patronymic || '',
                 user.lastName || ''
            ].join(' ')
        }

        return (<React.Fragment>
                <h1>{ident}</h1>
                <UserForm
                    onChange={v => this.updateFormData(v)}
                    onSubmit={v => this.updateUser(v)}
                    value={this.state.user}
                    accessRights={this.props.accessRights}
                    labels={this.props.labels}
                    />

            </React.Fragment>)
    }
}

const withDefaultProps = defaultProps({
    labels: {
        save: "Сохранить"
    },
    id: _.uniqueId(),
    accessRights: []
})

export default withDefaultProps(UpdateUser)