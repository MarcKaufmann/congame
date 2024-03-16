import { LitElement, css, html } from "lit";
import { customElement, property } from "lit/decorators.js";

interface Value {
  radioValue: string | null;
  otherValue: string | null;
}

interface Option {
  label: string;
  value: string;
}

@customElement("cg-radios-with-other")
export default class CgRadiosWithOther extends LitElement {
  @property({ type: String })
  name: string;

  @property({ converter: (text) => JSON.parse(text) })
  value: Value;

  @property({ converter: (text) => JSON.parse(text) })
  options: Option[];

  static formAssociated = true;
  static styles = css``;

  private internals: ElementInternals;

  constructor() {
    super();

    this.internals = this.attachInternals();
  }

  private onRadioChanged(e: Event) {
    this.value = {
      ...this.value,
      radioValue: (e.target as any).value,
      otherValue: null,
    };
    this.internals.setFormValue(JSON.stringify(this.value));
  }

  private onOtherChanged(e: Event) {
    this.value = {
      ...this.value,
      radioValue: null,
      otherValue: (e.target as any).value,
    };
    this.internals.setFormValue(JSON.stringify(this.value));
  }

  render() {
    const radios = this.options.map(({ label, value }) => {
      return html`<label
        ><input
          type="radio"
          name=${this.name}
          .value=${value}
          ?checked=${value === this.value.radioValue}
          @change=${(e: Event) => this.onRadioChanged(e)}
        />
        ${label}</label
      >`;
    });
    const other = html`<label
      ><input
        type="radio"
        name=${this.name}
        .value=${null}
        ?checked=${this.value.otherValue !== null}
        @change=${(e: Event) => this.onRadioChanged(e)} />
      Other:
      <input
        type="text"
        placeholder="Other..."
        .value=${this.value.otherValue ?? ""}
        ?required=${this.value.radioValue === null}
        @change=${(e: Event) => this.onOtherChanged(e)}
    /></label>`;

    return html`<div class="cg-radios-with-other">${radios} ${other}</div>`;
  }
}
