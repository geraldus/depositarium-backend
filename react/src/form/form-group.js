import React from 'react'
import { defaultProps } from 'recompose'
import _ from 'lodash'


export class FormGroup extends React.Component {
    constructor (props) {
        super(props)
        this.state = {
            value: ''
        }
    }
    updateValue (e) {
        console.log(e)
        this.setState(s => _.merge({}, s, { value: e.value }))
        this.props.onChange(e)
    }
    render () {
        const { id, label, name, type } = this.props
        return (
            <div id={id} className="form-group">
                {label && <label htmlFor={`${id}-name`}>{label}</label>}
                <input
                    id={name? `${id}-${name}` : undefined}
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
