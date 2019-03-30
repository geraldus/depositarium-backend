import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'


export class FormCheck extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            value: this.props.value,
            checked: this.props.checked
        }
    }
    updateValue (e) {
        const val = e.target.value
        this.setState(s => {
            this.props.onChange(val, !s.checked)
            return (_.merge({}, s, { value: val, checked: !s.checked }))
        })
    }
    render () {
        const { id, label, name, type } = this.props
        let id_ = []
        if (id != '') id_.push(id)
        if (name != '') id_.push(name)
        id_ = id.length > 0? id_.join('-') : undefined
        return (
            <div className="form-check">
                <input
                    id={id_}
                    className="form-check-input"
                    type={type}
                    name={name? name : undefined}
                    value={this.state.value}
                    defaultChecked={this.props.checked}
                    onChange={e => this.updateValue(e)} />
                {label && <label
                    htmlFor={`${id_}`}
                    className="form-check-label">{label}</label>}
            </div>
        )
    }
}

const withDefaultProps = defaultProps({
    id: _.uniqueId(),
    checked: false,
    name: ''
})

export default withDefaultProps(FormCheck)
