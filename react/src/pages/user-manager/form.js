import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'
import 'whatwg-fetch'

import FormGroup from '../../form/form-group'
import FormCheck from '../../form/form-check'


const mkFullName = (l, f, p) =>
        [ l, f, p ]
            .filter(x => x != '' && typeof x == "string")
            .join(' ')

const takeNamePieces = full => ({
    lastName: takeLastName(full),
    firstName: takeFirstName(full),
    patronymic: takePatronymic(full)
})

const takeFirstName = full => {
    const pieces = full.split(' ')
    switch (pieces.length) {
        case 0: return ''
        // No input
        case 1: return pieces[0]
        // [x] Anything
        default: return pieces[1]
        // [ ] Fayzrakhmanov [x] Arthur
        // [ ] Fayzrakhmanov [x] Arthur [ ] Sakhievich ...
    }
}

const takeLastName = full => {
    const pieces = full.split(' ')
    switch (pieces.length) {
        case 0:
        case 1: return ''
        // No input
        // [ ] Anything
        case 2: return pieces[0]
        // [x] Fayzrakhmanov [ ] Arthur
        default: return pieces[0]
        // [x] Fayzrakhmanov [ ] Arthur [ ] Sakhievich ...
    }
}

const takePatronymic = full => {
    const pieces = full.split(' ')
    switch (pieces.length) {
        case 0:
        case 1:
        case 2: return ''
        // No input
        // [ ] Anything
        // [ ] Fayzrakhmanov [ ] Arthur
        default: return pieces[2]
        // [ ] Fayzrakhmanov [ ] Arthur [x] Sakhievich ...
    }
}

const hasAccess = (origin, right) => origin.name === right

export class UserForm extends React.Component {
    constructor (props) {
        super(props)
        let v = props.value
        const fullName = mkFullName(v.lastName, v.firstName, v.patronymic)
        this.state = {
            fullName: fullName,
            ident: '',
            email: '',
            password: '',
            rights: [],
            ... this.props.value
        }
    }
    componentDidUpdate (prevProps, prevState) {
        if (
                typeof this.props.onChange == "function"
                && !_.isEqual(prevState, this.state)
            ) {
            const namePieces = takeNamePieces(this.state.fullName)
            this.props.onChange({
                ... this.state,
                ... namePieces
            })
        }
    }
    onSubmit (e) {
        if (typeof this.props.onSubmit == "function") {
            const { fullName } = this.state
            const namePieces = {
                lastName: takeLastName(fullName),
                firstName: takeFirstName(fullName),
                patronymic: takePatronymic(fullName)
            }
            const r = {
                ... this.state,
                ... namePieces
            }
            this.props.onSubmit(r)
        }
    }
    updateInputData (e, name) {
        this.setState(s => {
            if (typeof this.state[name] != "undefined") {
                let val = {}
                val[name] = e
                return _.merge({}, s, { ... val })
            } else {
                return _.merge({}, s)
            }
        })
    }
    toggleAccess (e, check, name) {
        this.setState(s => {
            const currentSet = s.rights
            let val = {}
            if (check) {
                val = [  ... new Set([ name, ... currentSet ]) ]
            } else {
                val = currentSet.filter(n => n != name)
            }
            return { rights: val }
        })
    }
    render () {
        const L = this.props.labels
        const { id, accessRights } = this.props
        return (<React.Fragment>
            <div className="row">
                <div className="col">
                    {L.formTitle}
                    <FormGroup
                        name="fullName"
                        label={L.nameTitle}
                        value={this.state.fullName}
                        onChange={e => this.updateInputData(e, "fullName")}/>
                    <FormGroup
                        name="ident"
                        label={L.identTitle}
                        value={this.state.ident}
                        onChange={e => this.updateInputData(e, "ident")}/>
                    <FormGroup
                        name="email"
                        label={L.emailTitle}
                        value={this.state.email}
                        onChange={e => this.updateInputData(e, "email")}/>
                    <FormGroup
                        name="password"
                        label={L.passwordTitle}
                        value={this.state.password}
                        onChange={e => this.updateInputData(e, "password")}/>
                    {accessRights.map((ar, idx) => {
                        return (<FormCheck
                            id={`${this.props.id}-${idx}`}
                            key={idx}
                            type="checkbox"
                            name="rights"
                            label={ar.name}
                            value={ar.name}
                            checked={this.state.rights.findIndex(x => hasAccess(ar, x)) != -1}
                            onChange={(e, c) => this.toggleAccess(e, c, ar.name)}
                            />)
                        })
                    }
                    {this.props.onDismiss && <button
                        className="btn btn-outline-secondary"
                        onClick={console.log}>
                        {L.cancel}
                        </button>}
                    {this.props.onSubmit && <button
                        className="btn btn-success"
                        onClick={e => this.onSubmit(e)}>
                        {L.save}
                        </button>}
                </div>
            </div>
        </React.Fragment>)
    }
}

const withDefaultProps = defaultProps({
    labels: {
        formTitle: <h3>Новый пользователь</h3>,
        nameTitle: "ФИО",
        identTitle: "Логин",
        emailTitle: "Эл.почта",
        passwordTitle: "Пароль",
        save: "Создать",
        cancel: "Отмена"
    },
    id: _.uniqueId(),
    accessRights: [],
    value: {
        firstName: '',
        lastName: '',
        patronymic: ''
    }
})

export default withDefaultProps(UserForm)