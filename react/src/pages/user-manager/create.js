import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'

import FormGroup from '../../form/form-group'
import FormCheck from '../../form/form-check'

export class CreateUser extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            name: '',
            ident: '',
            email: '',
            password: '',
            rights: []
        }
    }
    updateInputData (e, name) {

    }
    createUser (e) {
        e.preventDefault()
        const formData = new FormData()
        formData.append("name", this.state.name)
        formData.append("ident", this.state.ident)
        formData.append("email", this.state.email)
        formData.append("password", this.state.password)
        console.log(formData)
    }
    render () {
        const L = this.props.labels
        const { id, accessRights } = this.props
        return (<React.Fragment>
            <div className="row">
                <div className="col">
                    <FormGroup
                        name="name"
                        label={L.nameTitle}
                        value={this.state.name}/>
                    <FormGroup
                        name="ident"
                        label={L.identTitle}
                        value={this.state.ident}/>
                    <FormGroup
                        name="email"
                        label={L.emailTitle}
                        value={this.state.email}/>
                    <FormGroup
                        name="password"
                        label={L.passwordTitle}
                        value={this.state.password}/>
                    {accessRights.map((ar, idx) =>
                        <FormCheck
                            key={idx}
                            type="checkbox"
                            name="rights"
                            label={ar.name}
                            value={ar.name}
                            />)}
                    <button
                        className="btn btn-success"
                        onClick={e => this.createUser(e)}>
                        {L.save}
                        </button>
                </div>
            </div>
        </React.Fragment>)
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