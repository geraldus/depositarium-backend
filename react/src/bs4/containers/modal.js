import React from 'react'


/** Bootstrap 4 Modal container */
export class Bs4ModalContainer extends React.Component {
    render () {
        return (
            <div
                    id={this.props.id}
                    className={`modal${this.props.fade? ' fade' : ''}`}
                    tabIndex="-1"
                    role="dialog">
                <div className="modal-dialog" role="document">
                    <div className="modal-content">
                        {this.props.children}
                    </div>
                </div>
            </div>
        )
    }
}

/** Bootstrap 4 Modal Header container */
export class Bs4ModalHeaderContainer extends React.Component {
    render () {
        return (
            <div className="modal-header">
                <h5 className="modal-title">{this.props.title}</h5>
                <button
                        type="button"
                        className="close"
                        data-dismiss="modal"
                        aria-label={this.props.closeIconAriaLabel}>
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
        )
    }
}

/** Bootstrap 4 Modal Body container */
export class Bs4ModalBodyContainer extends React.Component {
    render () {
        return (
            <div className="modal-body">
                {this.props.children}
            </div>
        )
    }
}

/** Bootstrap 4 Modal Footer container */
export class Bs4ModalFooterContainer extends React.Component {
    render () {
        return (
            <div className="modal-footer">
                {this.props.children}
            </div>
        )
    }
}
