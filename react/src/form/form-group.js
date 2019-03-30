import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'


export class FormGroup extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            value: this.props.value
        }
    }
    updateValue (e) {
        const val = e.target.value
        this.setState(s => _.merge({}, s, { value: val }))
        this.props.onChange(val)
    }
    render () {
        const { id, label, name, type } = this.props
        let id_ = []
        if (id != '') id_.push(id)
        if (name != '') id_.push(name)
        id_ = id.length > 0? id_.join('-') : undefined
        return (
            <div id={id_} className="form-group">
                {label && <label htmlFor={id_}>{label}</label>}
                <input
                    id={id_}
                    className="form-control"
                    type={type}
                    name={name? name : undefined}
                    value={this.state.value}
                    onChange={e => this.updateValue(e)} />
            </div>
        )
    }
}

const withDefaultProps = defaultProps({
    id: _.uniqueId(),
    name: '',
    type: 'text'
})

export default withDefaultProps(FormGroup)
